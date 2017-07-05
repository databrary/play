package routes

import (
	"github.com/databrary/databrary/db"
	public_models "github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/util"
	"github.com/databrary/scs/session"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"github.com/pressly/chi"
	"net/http"
)

func volume(r chi.Router) {

	r.With(IsLoggedInHandler).Group(func(r chi.Router) {
		r.Route("/", func(r chi.Router) {
			r.Get("/all", GetUserVolumes)
		})
	})
}

func GetUserVolumes(w http.ResponseWriter, r *http.Request) {
	var (
		err       error
		conn      *sqlx.DB
		va        public_models.VolumeAccessSlice
		accountId int
	)

	nInfo := NetInfoLogEntry(r)

	if accountId, err = session.GetInt(r, "account_id"); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find account %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if conn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db conn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	if va, err = public_models.VolumeAccesses(conn,
		qm.Where("individual = $1", public_models.PermissionADMIN),
		qm.And("party = $2", accountId),
		qm.Load("Volume"), //this is key for getting R to work. this is the name of relationship you want to eagerly load
	).All(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't find volume accesses %d", accountId)
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	volumes := public_models.VolumeSlice{}

	for _, v := range va {
		volumes = append(volumes, v.R.Volume)
	}

	util.WriteJSONResp(w, "ok", volumes)
}
