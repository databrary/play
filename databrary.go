// main.go
package main

import (
	"net/http"

	"os"
	"path/filepath"

	"fmt"
	"github.com/databrary/databrary/config"
	"github.com/databrary/databrary/db"
	"github.com/databrary/databrary/logging"
	"github.com/databrary/databrary/routes"
	"github.com/databrary/databrary/services/redis"
	"github.com/databrary/databrary/services/sessions"
	"github.com/databrary/sqlboiler/boil"
	"github.com/pressly/chi"
	"github.com/pressly/chi/docgen"
	"github.com/pressly/chi/middleware"
	"github.com/rs/cors"
	"github.com/unrolled/secure"
	"gopkg.in/alecthomas/kingpin.v2"
	"regexp"
	"strings"
	"time"
)

var (
	proj_root   string
	config_path *string
)

func init() {
	goPaths := strings.Split(filepath.Join(os.Getenv("GOPATH"), "src/github.com/databrary/databrary/"), ":")
	if len(goPaths) == 2 {
		proj_root = goPaths[1]
	} else if len(goPaths) == 1 {
		proj_root = goPaths[0]
	} else {
		panic(fmt.Sprintf("unexpected gopath %#v", goPaths))
	}
	config_path = kingpin.Flag("config", "Path to config file").
		Default(filepath.Join(proj_root, "config/databrary_dev.toml")).
		Short('c').
		String()
	// cmd line flags
	kingpin.Version("0.0.0")
	kingpin.Parse()

	if config_path, err := filepath.Abs(*config_path); err != nil {
		panic("command line config file path error")
	} else {
		log.InitLgr(config.InitConf(config_path))
	}

	err := db.InitDB(config.GetConf())
	if err != nil {
		panic(err.Error())
	}

	redis.InitRedisStore(config.GetConf())

	if config.GetConf().GetString("log.level") == "DEBUG" {
		boil.DebugMode = true
	}
}

func main() {
	// New permissions middleware
	conf := config.GetConf()
	secureMiddleware := secure.New(secure.Options{
		AllowedHosts:          []string{"localhost:3000", "localhost:3444", "www.petri.li:3000"},
		HostsProxyHeaders:     []string{"X-Forwarded-Host"},
		SSLRedirect:           true,
		SSLHost:               "",
		SSLProxyHeaders:       map[string]string{"X-Forwarded-Proto": "https"},
		STSSeconds:            315360000,
		STSIncludeSubdomains:  true,
		STSPreload:            true,
		FrameDeny:             true,
		ContentTypeNosniff:    true,
		BrowserXssFilter:      true,
		ContentSecurityPolicy: "default-src 'self'",
		PublicKey:             `pin-sha256="base64+primary=="; pin-sha256="base64+backup=="; max-age=5184000; includeSubdomains; report-uri="https://www.example.com/hpkp-report"`,
		IsDevelopment:         true,
	})

	r := chi.NewRouter()
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(log.NewStructuredLogger(log.Logger))
	r.Use(middleware.Recoverer)
	r.Use(secureMiddleware.Handler)
	r.Use(middleware.Timeout(60 * time.Second))

	rateLimiter, err := routes.NewRateLimiter()
	if err != nil {
		log.WrapErrLogFatal(err, "couldn't create rate limiter")
	}

	r.Use(rateLimiter.RateLimit)
	c := cors.New(cors.Options{
		AllowedOrigins:   []string{"*"}, // TODO: []string{"http://localhost:3000", "https://localhost:3000"},
		AllowCredentials: true,
		AllowedMethods:   []string{"GET", "POST", "OPTIONS", "PATCH"},
		AllowedHeaders:   []string{"Content-Type", "Authorization"},
		Debug:            true,
	})

	r.Use(c.Handler)
	r.Use(sessions.NewSessionManager())

	r.Use(middleware.StripSlashes)
	r.Mount("/api", routes.Api())

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("nothing here yet"))
	})

	r.FileServer("/public", http.Dir("public"))
	GenerateApi(r)
	addr := conf.GetString("address.domain") + ":" + conf.GetString("address.backend_port")
	fmt.Printf("serving on %s://%s/\n", conf.GetString("address.scheme"), addr)

	certPath := conf.GetString("ssl.cert")
	keyPath := conf.GetString("ssl.key")
	err = http.ListenAndServeTLS(addr, certPath, keyPath, r)
	fmt.Println(err)
}

func GenerateApi(r chi.Router) {
	m := docgen.MarkdownOpts{
		ProjectPath:        "github.com/databrary/databrary",
		Intro:              "Databrary 2.0 API",
		ForceRelativeLinks: true,
	}
	// skip middleware
	re := regexp.MustCompile(`^- \[`)
	f, _ := os.Create(filepath.Join(proj_root, "api.md"))
	defer f.Close()
	docs := docgen.MarkdownRoutesDoc(r, m)
	for _, v := range strings.Split(docs, "\n") {
		if re.FindStringIndex(v) == nil {
			f.WriteString(v + "\n")
		}
	}
	f.Sync()
}
