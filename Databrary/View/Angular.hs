{-# LANGUAGE CPP, OverloadedStrings #-}
module Databrary.View.Angular
  ( htmlAngular
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (def)
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Service.Types
import Databrary.Model.Identity
import Databrary.Action.Types
import Databrary.Web (WebFilePath (..))
import Databrary.Controller.Web
import Databrary.View.Html
import Databrary.View.Template

ngAttribute :: String -> H.AttributeValue -> H.Attribute
ngAttribute = H.customAttribute . H.stringTag . ("ng-" <>)

webURL :: BS.ByteString -> H.AttributeValue
webURL p = actionValue webFile (Just $ StaticPath p) ([] :: Query)

htmlAngular :: [WebFilePath] -> [WebFilePath] -> BSB.Builder -> RequestContext -> H.Html
htmlAngular cssDeps jsDeps nojs auth = H.docTypeHtml H.! ngAttribute "app" "databraryModule" $ do
  H.head $ do
    htmlHeader Nothing def
    H.noscript $
      H.meta
        H.! HA.httpEquiv "Refresh"
        H.! HA.content (builderValue $ BSB.string8 "0;url=" <> nojs)
    H.meta
      H.! HA.httpEquiv "X-UA-Compatible"
      H.! HA.content "IE=edge"
    H.meta
      H.! HA.name "viewport"
      H.! HA.content "width=device-width, initial-scale=1.0, minimum-scale=1.0"
    H.title
      H.! ngAttribute "bind" (byteStringValue $ "page.display.title + ' || " <> title <> "'")
      $ H.unsafeByteString title
    forM_ [Just "114x114", Just "72x72", Nothing] $ \size ->
      H.link
        H.! HA.rel "apple-touch-icon-precomposed"
        H.! HA.href (webURL $ "icons/apple-touch-icon" <> maybe "" (BSC.cons '-') size <> ".png")
        !? (HA.sizes . byteStringValue <$> size)
    forM_ cssDeps $ \css ->
      H.link
        H.! HA.rel "stylesheet"
        H.! HA.href (webURL $ webFileRel css)
    H.script $ do
      H.preEscapedString "(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer','GTM-NW6PSFL');"
    H.script $ do
      H.preEscapedString "window.$play={user:"
      unsafeBuilder $ JSON.fromEncoding $ JSON.recordEncoding $ identityJSON (view auth)
      forM_ (serviceDown (view auth)) $ \msg -> do
        H.preEscapedString ",down:"
        H.unsafeLazyByteString $ JSON.encode msg
      H.preEscapedString "};"
    forM_ jsDeps $ \js ->
      H.script
        H.! HA.src (webURL $ webFileRel js)
        $ return ()
  H.body
    H.! H.customAttribute "flow-prevent-drop" mempty
    $ do
    H.noscript $ do
      H.preEscapedString "Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others) with Javascript enabled.  You can also switch to the "
      H.a
        H.! HA.href (builderValue nojs)
        $ "simple version"
      H.preEscapedString " of this page."
    H.preEscapedString "<toolbar></toolbar>"
    H.preEscapedString $ "<main ng-view id=\"main\" class=\"main"
#ifdef SANDBOX
      <> " sandbox"
#endif
      <> "\" autoscroll ng-if=\"!page.display.error\"></main>"
    H.preEscapedString "<errors></errors>"
    htmlFooter
    H.preEscapedString "<messages></messages>"
    H.preEscapedString "<tooltip ng-repeat=\"tooltip in page.tooltips.list\"></tooltip>"
    H.div
      H.! HA.id "loading"
      H.! HA.class_ "loading"
      H.! HA.style "display:none"
      H.! ngAttribute "show" "page.display.loading" $
      H.div H.! HA.class_ "loading-animation" $ do
        H.div H.! HA.class_ "loading-spinner" $
          H.div H.! HA.class_ "loading-mask" $
            H.div H.! HA.class_ "loading-circle" $
              return ()
        H.div H.! HA.class_ "loading-text" $
          "[" >> H.span "loading" >> "]"
    H.script
      $ H.preEscapedString "document.getElementById('loading').style.display='block';"
    H.script
      $ H.preEscapedString "function initMap(){var n,i=new google.maps.LatLngBounds;n=new google.maps.Map(document.getElementById(\"map_canvas\"),{mapTypeId:\"roadmap\"});var e,o,t=[[\"New York University\",40.72951339999999,-73.99646089999999,\"collecting\"],[\"Georgetown University\",38.9076089,-77.07225849999998,\"collecting\"],[\"Cornell University\",42.4534492,-76.47350269999998,\"collecting\"],[\"Purdue University\",40.4237054,-86.92119459999998,\"collecting\"],[\"Virginia Commonwealth University\",37.5488396,-77.45272720000003,\"collecting\"],[\"University of Oregon\",44.0448302,-123.07260550000001,\"collecting\"],[\"University of California at Riverside\",33.9737055,-117.32806440000002,\"collecting\"],[\"Stanford University\",37.4274745,-122.16971899999999,\"collecting\"],[\"Boston University\",42.3504997,-71.1053991,\"collecting\"],[\"Penn State University\",40.7982133,-77.8599084,\"collecting\"],[\"California State University at Long Beach\",33.7838235,-118.11409040000001,\"collecting\"],[\"Ohio State University\",40.0141905,-83.0309143,\"collecting\"],[\"University of Pittsburgh\",40.4443533,-79.96083499999997,\"collecting\"],[\"CUNY -- College of Staten Island\",40.6018152,-74.14849040000001,\"collecting\"],[\"California State University at Fullerton\",33.8829226,-117.88692609999998,\"collecting\"],[\"Michigan State University\",42.701848,-84.48217190000003,\"collecting\"],[\"University of Texas at Austin\",30.29128,-97.73858,\"collecting\"],[\"Princeton University\",40.3439888,-74.65144809999998,\"collecting\"],[\"Rutgers University\",40.741187,-74.17530950000003,\"collecting\"],[\"Tulane University\",29.9403477,-90.12072790000002,\"collecting\"],[\"University of Miami\",25.7904064,-80.21199279999996,\"collecting\"],[\"Emory University\",33.7925195,-84.32399889999999,\"collecting\"],[\"Vanderbilt University\",36.1447034,-86.80265509999998,\"collecting\"],[\"University of California at Davis\",38.5382322,-121.76171249999999,\"collecting\"],[\"Children's Hospital of Philadelphia\",39.9489145,-75.1939595,\"collecting\"],[\"College of William & Mary\",37.271674,-76.71337799999998,\"collecting\"],[\"University of California at Santa Cruz\",36.9914738,-122.05829719999997,\"collecting\"],[\"University of California at Merced\",37.3648748,-120.42540020000001,\"collecting\"],[\"University of Houston\",29.7199489,-95.3422334,\"collecting\"],[\"Indiana University\",39.1745704,-86.51294580000001,\"collecting\"],[\"University of Georgia\",33.9480053,-83.37732210000001,\"collecting\"],[\"University of Chicago\",41.7886079,-87.59871329999999,\"collecting\"],[\"Brown University\",41.8267718,-71.40254820000001,\"noncollecting\"],[\"Penn State University\",40.8,-77.7,\"noncollecting\"],[\"NICHD\",39.0292164,-77.13551009999998,\"noncollecting\"],[\"Ryerson University\",43.6576585,-79.3788017,\"noncollecting\"],[\"Arizona State University\",33.4242399,-111.92805269999997,\"noncollecting\"],[\"Lehigh University\",40.6069087,-75.3782832,\"noncollecting\"],[\"Texas A&M\",30.618531,-96.336499,\"noncollecting\"],[\"Penn State University\",40.9,-77.6,\"noncollecting\"],[\"Penn State University\",41,-77.5,\"noncollecting\"],[\"University of California at Riverside\",34,-117.2,\"noncollecting\"],[\"Cornell University\",42.5,-76.3,\"noncollecting\"],[\"University of Chicago\",41.8,-87.5,\"noncollecting\"],[\"Cornell University\",42.6,-76.2,\"noncollecting\"],[\"Purdue University\",40.5,-86.8,\"noncollecting\"],[\"Williams College\",42.7128038,-73.20302140000001,\"noncollecting\"],[\"Michigan State University\",42.8,-84.3,\"noncollecting\"],[\"University of Waterloo\",43.4722854,-80.5448576,\"noncollecting\"],[\"Temple University\",39.9811935,-75.15535119999998,\"noncollecting\"],[\"University of Pittsburgh\",40.5,-79.8,\"noncollecting\"],[\"Carnegie Mellon University\",40.4428081,-79.94301280000002,\"noncollecting\"],[\"University of Connecticut\",41.8077414,-72.25398050000001,\"noncollecting\"],[\"University of Washington\",47.65533509999999,-122.30351989999997,\"noncollecting\"],[\"Penn State University\",41.1,-77.4,\"noncollecting\"],[\"University of Illinois at Urbana-Champagne\",40.1019523,-88.22716149999997,\"noncollecting\"],[\"Harvard University\",42.3770029,-71.11666009999999,\"noncollecting\"],[\"University of Toronto Scarborough\",43.6628917,-79.39565640000001,\"noncollecting\"],[\"University of Connecticut\",41.9,-72.1,\"noncollecting\"],[\"University of Manitoba\",49.8075008,-97.13662590000001,\"noncollecting\"],[\"Brooklyn College\",40.6314406,-73.95444880000002,\"noncollecting\"],[\"New York University\",40.8,-73.8,\"noncollecting\"],[\"University of California at Merced\",37.4,-120.3,\"noncollecting\"],[\"University of Texas at Austin\",30.3,-97.6,\"noncollecting\"],[\"Temple University\",40,-75,\"noncollecting\"]],l=new google.maps.InfoWindow;for(o=0;o<t.length;o++){var c=new google.maps.LatLng(t[o][1],t[o][2]);i.extend(c),e=new google.maps.Marker({position:c,map:n,title:t[o][0],icon:\"/web/icons/marker-\"+t[o][3]+\".png\"}),google.maps.event.addListener(e,\"mouseover\",function(i,e){return function(){l.setContent('<div class=\"info_content\">'+t[e][0]+\"</div>\"),l.open(n,i)}}(e,o)),n.fitBounds(i)}var r=google.maps.event.addListener(n,\"bounds_changed\",function(n){this.setCenter(new google.maps.LatLng(41.4925374,-99.90181310000003)),this.setZoom(4),google.maps.event.removeListener(r)})}"
  where
  title =
#ifdef SANDBOX
    "Databrary Demo"
#else
    "Databrary"
#endif
