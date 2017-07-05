package routes

import (
	"encoding/json"
	"github.com/databrary/databrary/db"
	"github.com/databrary/databrary/db/models/sqlboiler_models/public"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/services/mail"
	"github.com/databrary/databrary/util"
	"github.com/databrary/sqlboiler/queries/qm"
	"github.com/jmoiron/sqlx"
	"github.com/pressly/chi"
	"github.com/renstrom/fuzzysearch/fuzzy"
	"github.com/sirupsen/logrus"
	"gopkg.in/olahol/melody.v1"
	"io/ioutil"
	"net/http"
	"sort"
	"sync"
)

// A completely separate router for administrator routes
func Api() http.Handler {
	r := chi.NewRouter()
	r.Route("/user", user)
	r.Get("/loggedin", IsLoggedInEndpoint)
	r.Post("/report-error", ReportError)
	r.Get("/autocomplete-affil", AutoCompleteAffil)
	r.Get("/site-stats", GetSiteStats)
	return r
}

func NetInfoLogEntry(r *http.Request) *logrus.Entry {
	fields := logrus.Fields(map[string]interface{}{
		"uri": r.RequestURI,
		"ip":  r.RemoteAddr,
	})
	return log.Logger.WithFields(fields)
}

func user(r chi.Router) {

	r.Post("/login", PostLogin)
	r.Post("/logout", PostLogOut)
	r.Get("/login", GetLogin) //TODO remove

	r.Post("/reset-password/email", ResetPasswordEmail)
	r.Post("/reset-password/token", ResetPasswordToken)

	r.Get("/exists", UserExists)
	r.Post("/check-token", CheckTokenExpiryEndpoint)

	r.Post("/register", Register)

	r.With(IsLoggedInHandler).Group(func(r chi.Router) {
		r.Route("/profile", func(r chi.Router) {
			r.Get("/", GetProfile)
			r.Patch("/", PatchProfile)
		})
		r.Route("/volume", volume)
	})

}

func AutoCompleteAffil(w http.ResponseWriter, r *http.Request) {
	mrouter := melody.New()
	lock := new(sync.Mutex)
	nInfo := NetInfoLogEntry(r)
	mrouter.HandleConnect(func(s *melody.Session) {

		dbConn, err := db.GetDbConn()

		if err != nil {
			log.EntryWrapErr(nInfo, err, "couldn't open db conn")
			return
		}

		affilsPartySlice, err := public.Parties(dbConn, qm.Select("distinct affiliation"), qm.Where("affiliation IS NOT NULL")).All()
		if err != nil {
			log.EntryWrapErr(nInfo, err, "couldn't get affils")
			return
		}

		affilsString := make([]string, len(affilsPartySlice))
		for _, affil := range affilsPartySlice {
			affilsString = append(affilsString, affil.Affiliation.String)
		}
		lock.Lock()
		s.Set("affiliations", affilsString)
		lock.Unlock()

	})

	mrouter.HandleMessage(func(s *melody.Session, affil []byte) {
		affiliations, err := s.Get("affiliations")
		if err == false {
			log.EntryWrapErr(nInfo, nil, "couldn't get affils")
			return
		}
		matchingAffiliations := fuzzy.RankFindFold(string(affil), affiliations.([]string))
		sort.Sort(matchingAffiliations)
		lenResults := 20
		if matchingAffiliations.Len() < 20 {
			lenResults = matchingAffiliations.Len()
		}
		matchingAffiliations = matchingAffiliations[:lenResults]
		j, _ := json.Marshal(matchingAffiliations)
		mrouter.Broadcast(j)
	})

	mrouter.HandleClose(func(m *melody.Session, i int, s string) error {
		mrouter.Close()
		return nil
	})

	mrouter.HandleError(func(s *melody.Session, err error) {
		mrouter.Close()
	})
	err := mrouter.HandleRequest(w, r)
	if err != nil {
		log.EntryWrapErr(nInfo, err, "couldn't handle websocket request")
	}
}

func ReportError(w http.ResponseWriter, r *http.Request) {
	nInfo := NetInfoLogEntry(r)
	body, err := ioutil.ReadAll(r.Body)
	if err == nil {
		mail.SendEmail(string(body), "Databrary Error", "maksim.levental@nyu.edu")
	} else {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't send report error email")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
	}

}

func GetSiteStats(w http.ResponseWriter, r *http.Request) {
	nInfo := NetInfoLogEntry(r)
	/*
			  ac <- dbQuery [pgSQL|SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site|]
		  v <- dbQuery1' [pgSQL|SELECT count(id) FROM volume WHERE id > 0|]
		  vs <- dbQuery1' [pgSQL|SELECT count(volume) FROM volume_access WHERE volume > 0 AND party = 0 AND children >= 'PUBLIC'|]
		  (a, ad, ab) <- dbQuery1' [pgSQL|SELECT count(id), sum(duration), sum(size) FROM asset JOIN slot_asset ON asset = id WHERE volume > 0|]
		  rc <- dbQuery [pgSQL|SELECT category, count(id) FROM record GROUP BY category ORDER BY category|]
	*/

	data := struct {
		Authorized []struct {
			Site  string `json:"site"`
			Total int    `json:"total"`
		} `json:"authorized"`
		Volumes       int `json:"volumes"`
		PublicVolumes int `json:"public_volumes"`
		Asset         struct {
			Total       int     `json:"total"`
			SumDuration string  `json:"sum_duration"`
			SumSize     float64 `json:"sum_size"`
		} `json:"asset"`
		Record []struct {
			Category string `json:"category"`
			Total    string `json:"total"`
		} `json:"record"`
	}{}

	var (
		dbConn *sqlx.DB
		err    error
	)

	if dbConn, err = db.GetDbConn(); err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't open db dbConn")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	rows, err := dbConn.Query("SELECT site, count(child) FROM authorize_view WHERE parent = 0 AND child > 4 GROUP BY site")

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get authorized investigators")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	for rows.Next() {
		authorize := struct {
			Site  string `json:"site"`
			Total int    `json:"total"`
		}{}
		err = rows.Scan(&authorize.Site, &authorize.Total)
		if err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get authorized investigators")
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		}
		data.Authorized = append(data.Authorized, authorize)
	}

	rows, err = dbConn.Query("SELECT count(id) FROM volume WHERE id > 0")

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get volumes")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	for rows.Next() {
		err = rows.Scan(&data.Volumes)
		if err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get volumes")
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		}
	}

	rows, err = dbConn.Query("SELECT count(volume) FROM volume_access WHERE volume > 0 AND party = 0 AND children >= 'PUBLIC'")

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get public volumes")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	for rows.Next() {
		err = rows.Scan(&data.PublicVolumes)
		if err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get public volumes")
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		}
	}

	rows, err = dbConn.Query("SELECT count(id), sum(duration), sum(size) FROM asset JOIN slot_asset ON asset = id WHERE volume > 0")

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get participants")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	for rows.Next() {
		err = rows.Scan(&data.Asset.Total, &data.Asset.SumDuration, &data.Asset.SumSize)
		if err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get participants")
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		}
	}

	rows, err = dbConn.Query("SELECT name, count(record.id) FROM record join category on category = category.id GROUP BY name ORDER BY name")

	if err != nil {
		_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get records")
		util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
		return
	}

	for rows.Next() {
		record := struct {
			Category string `json:"category"`
			Total    string `json:"total"`
		}{}
		if err != nil {
			_, errorUuid := log.EntryWrapErr(nInfo, err, "couldn't get records")
			util.JsonErrResp(w, http.StatusInternalServerError, errorUuid)
			return
		}
		err = rows.Scan(&record.Category, &record.Total)
		data.Record = append(data.Record, record)
	}

	util.WriteJSONResp(w, "ok", data)
}

//func adminRouter() http.Handler {
//	r := chi.NewRouter()
//	r.Use(AdminOnly)
//	r.Get("/", adminIndex)
//	r.Get("/accounts", adminListAccounts)
//	return r
//}
