package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import               Forms._
import          i18n.Messages
import          libs.Files.TemporaryFile
import          libs.iteratee.Enumerator
import          libs.concurrent.Execution.Implicits.defaultContext
import util._
import models._
import dbrary.Offset

object Asset extends SiteController {

  private[controllers] def checkContainer(v : models.Volume.Id, i : models.Container.Id, a : models.Asset.Id, p : Permission.Value = Permission.VIEW)(act : ContainerAsset => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    ContainerAsset.get(a, i).filter(_.volumeId == v).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  private[controllers] def checkSlot(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, p : Permission.Value = Permission.VIEW)(act : SlotAsset => SiteRequest[AnyContent] => Result) = SiteAction { implicit request =>
    SlotAsset.get(o, i).filter(_.volumeId == v).fold(NotFound : Result) { link =>
      if (link.permission < p)
        Forbidden
      else
        act(link)(request)
    }
  }

  def view(v : models.Volume.Id, i : models.Slot.Id, a : models.Asset.Id) = checkSlot(v, i, a) { link => implicit request =>
    Ok(views.html.asset.view(link))
  }

  private def assetResult(tag : String, data_ : => Future[store.StreamEnumerator], fmt : AssetFormat, saveAs : Option[String])(implicit request : SiteRequest[_]) : Result = {
    /* The split works because we never use commas within etags. */
    val ifNoneMatch = request.headers.getAll(IF_NONE_MATCH).flatMap(_.split(',').map(_.trim))
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (ifNoneMatch.exists(t => t.equals("*") || HTTP.unquote(t).equals(tag)) ||
      ifNoneMatch.isEmpty && request.headers.get(IF_MODIFIED_SINCE).isDefined)
      NotModified
    else AsyncResult(data_.map { data =>
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(CONTENT_LENGTH -> subdata.size.toString),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + data.size.toString)),
        Some(CONTENT_TYPE -> fmt.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + fmt.extension.fold("")("." + _)))),
        Some(ETAG -> HTTP.quote(tag)),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
        SimpleResult(
          header = ResponseHeader(range.fold(OK)(r => if (r._1 >= size) REQUESTED_RANGE_NOT_SATISFIABLE else PARTIAL_CONTENT),
            Map(headers : _*)),
          subdata)
      })
  }

  def download(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, inline : Boolean) = checkSlot(v, i, o, Permission.DOWNLOAD) { link => implicit request =>
    assetResult(
      "sobj:%d:%d".format(link.slotId.unId, link.link.assetId.unId),
      store.Asset.read(link),
      link.link.asset.format,
      if (inline) None else Some(link.link.name)
    )
  }

  def frame(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id, offset : Offset = 0) = checkSlot(v, i, o, Permission.DOWNLOAD) { link => implicit request =>
    link match {
      case ts : SlotTimeseries if offset >= 0 && offset < ts.duration =>
        assetResult(
          "sframe:%d:%d:%d".format(link.slotId.unId, link.link.assetId.unId, offset.millis.toLong),
          store.Asset.readFrame(ts, offset),
          ts.source.format.sampleFormat,
          None
        )
      case _ => NotFound
    }
  }
  def head(v : models.Volume.Id, i : models.Slot.Id, o : models.Asset.Id) = frame(v, i, o)

  type AssetForm = Form[(String, String, Option[Offset], Option[(Option[AssetFormat.Id], Classification.Value, Option[String], Unit)])]
  private[this] def assetForm(file : Boolean) : AssetForm = Form(tuple(
    "name" -> nonEmptyText,
    "body" -> text,
    "offset" -> optional(of[Offset]),
    "" -> MaybeMapping(if (file) Some(tuple(
      "format" -> optional(of[AssetFormat.Id]),
      "classification" -> Field.enum(Classification),
      "localfile" -> optional(nonEmptyText),
      "file" -> ignored(()))) else None)
  ))

  private[this] def formFill(link : ContainerAsset)(implicit site : Site) : AssetForm = {
    /* TODO Under what conditions should FileAsset data be allowed to be changed? */
    assetForm(false).fill((link.name, link.body.getOrElse(""), link.position, None))
  }

  def formForFile(form : AssetForm, target : Either[Container,ContainerAsset]) =
    form.value.fold(target.isLeft)(_._4.isDefined)

  def edit(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = checkContainer(v, s, o, Permission.EDIT) { link => implicit request =>
    Ok(views.html.asset.edit(Right(link), formFill(link)))
  }

  def change(v : models.Volume.Id, s : models.Container.Id, o : models.Asset.Id) = checkContainer(v, s, o, Permission.EDIT) { link => implicit request =>
    formFill(link).bindFromRequest.fold(
      form => BadRequest(views.html.asset.edit(Right(link), form)), {
      case (name, body, position, file) =>
        link.change(name = name, body = maybe(body), position = position)
        /* file foreach {
          () => link.asset.asInstanceOf[models.FileAsset].change
        } */
        Redirect(link.container.fullSlot.pageURL)
      }
    )
  }

  private[this] val uploadForm = assetForm(true)

  def create(v : models.Volume.Id, c : models.Container.Id, offset : Option[Offset]) = Container.check(v, c, Permission.CONTRIBUTE) { container => implicit request =>
    Ok(views.html.asset.edit(Left(container), uploadForm.fill(("", "", offset, Some((None, Classification.IDENTIFIED, None, ()))))))
  }

  def upload(v : models.Volume.Id, c : models.Container.Id) = Container.check(v, c, Permission.CONTRIBUTE) { container => implicit request =>
    def error(form : AssetForm) : Result =
      BadRequest(views.html.asset.edit(Left(container), form))
    val form = uploadForm.bindFromRequest
    form.fold(error _, {
      case (name, body, position, Some((format, classification, localfile, ()))) =>
        val ts = request.isAdmin
        val fmt = format.filter(_ => ts).flatMap(AssetFormat.get(_, ts))
        type ER = Either[AssetForm,(TemporaryFile,AssetFormat,String)]
        request.body.asMultipartFormData.flatMap(_.file("file")).fold {
          localfile.filter(_ => ts).fold(
            Left(form.withError("file", "error.required")) : ER) { localfile =>
            val file = new java.io.File(localfile)
            val name = file.getName
            if (file.isFile)
              (fmt orElse AssetFormat.getFilename(name, ts)).fold(Left(form.withError("format", "Unknown format")) : ER)(
                fmt => Right((TemporaryFile(file), fmt, name)))
            else
              Left(form.withError("localfile", "File not found"))
          }
        } { file =>
          (fmt orElse AssetFormat.getFilePart(file, ts)).fold(
            Left(form.withError("file", "file.format.unknown", file.contentType.getOrElse("unknown"))) : ER)(
            fmt => Right((file.ref, fmt, file.filename)))
        }.fold(error _, {
          case (file, fmt, fname) =>
            val asset = fmt match {
              case fmt : TimeseriesFormat if ts => // "if ts" should be redundant
                val probe = media.AV.probe(file.file)
                models.Timeseries.create(fmt, classification, probe.duration, file)
              case _ =>
                models.FileAsset.create(fmt, classification, file)
            }
            val link = ContainerAsset.create(container, asset, position, maybe(name).getOrElse(fname), maybe(body))
            Redirect(link.container.fullSlot.pageURL)
//            Redirect(link.container.pageURL)
        })
      case _ => error(uploadForm) /* should not happen */
      }
    )
  }

  def remove(v : models.Volume.Id, c : models.Container.Id, a : models.Asset.Id) = checkContainer(v, c, a, Permission.EDIT) { link => implicit request =>
    link.remove
    Redirect(link.container.fullSlot.pageURL)
  }
}
