{ name ? "databrary.conf"

, serverPort ? "8888"

, dbUser ? "databrary" # TODO this should match the user in db.nix
, dbPass ? "databrary123" # TODO this should match the user in db.nix
, dbName ? "databrary" # TODO this should match the user in db.nix

, dbHost ? "localhost"
, dbPort ? "5432"

# , dbPoolSize ? "5"
, databraryRoot ? ../databrary
, pkgs ? (import ./reflex-platform {}).nixpkgs

, solrPort ? "8984"
}: pkgs.writeText name ''
# Example settings for databrary.conf
# This file will be looked for in the current directory whenever databrary is run, or where specied by the -c flag.

## Secret key used to sign tokens, cookies, etc.  Should be long random string in production environment.
secret = "bob"
## Port to listen on (as http or https, as per ssl settings)
port = ${serverPort}
ssl {
  ## SSL certificate and private key for https.  If these are not specified, http will be used.
  ## these should be in the same directory as this conf file
  #cert = ["certificate.pem", "chain1.pem", ...]
  #key = "key.pem"
}
log {
  ## Where to log general messages
  messages {
    ## this is an absolute path or a std file descriptor
    file = "stderr"
    rotate = 100
  }
  ## Where to log all HTTP access
  access {
    ## this is an absolute path or a std file descriptor
    file = "stdout"
    ##
    rotate = 100
  }
}
db {
  ## Host and port, or socket path of postgres
  host = "${dbHost}"
  port = ${dbPort}

  ## unix socket if your postgres server isn't listening on a tcp port
  #sock = "/var/run/postgresql/.s.PGSQL.5432"
  ## Database user and password (if necessary), which must already exist in postgres

  user = "${dbUser}"
  pass = "${dbPass}"
  db = "${dbName}"

  ## Database name
  ## Verbosely log all database activity
  ## this is a very very useful flag. uncomment it and you'll see all of the
  ## template haskell generated sql queries
  #debug = true
}

store {
  ## uncomment this message and the app will display it (appropriately formatted and styled) instead of the frontpage
  #DOWN = "Databrary is unavailable due to NYU network issues. We expect full service to be restored shortly."

  master = "${databraryRoot}/databrary-db"
  #fallback
  upload = "${databraryRoot}/upload"
  temp = "${databraryRoot}/tmp"
  stage = "${databraryRoot}/stage"
  cache = "${databraryRoot}/cache"
  ## if this block is commented then transcoding will happen locally
  transcode {
     ## the ssh host that does the transcoding. this corresponds to an .ssh/hosts entry
     #host = "hpc"
     ## the mount point of hpc stuff. on our system this is at /nyu/hpc and then there's a symlink
     ## in the same directory as this conf file. generally it can be an absolute path
     #mount = "${databraryRoot}/nyu/hpc"
     ## the directory inside the mount folder
     #dir = "databrary"
  }
}

solr {
  ## Path to solr binary, defaulting to "solr" in PATH.
  #bin = "/usr/local/bin/solr"
  ## Alternatively, a host to connect to an already-running solr (untested).
  run = false
  host = "localhost"
  ## Port solr should listen on.
  port = ${solrPort}
  ## Directory to store solr cores.
  home = "${databraryRoot}/solr"
  ## Name of solr core to use.
  core = "databrary_core"
  ## Log file for solr
  log = "${databraryRoot}/databrary_logs/solr_log"
}

static {
  ## Email to send unknown authorization requests to
  authorize = "bob@nyu.edu"
  ## Email to send volume curation assistance requests to
  assist = "bob@nyu.edu"
  ## Remote service to generate investigator agreements during registration (see www/databrary/internal)
  #fillin = "http://databrary.org/internal/investigator.cgi"
  ## Key to use to authenticate to fillin service.
  #key = ""
}
ezid {
  ## Shoulder for ezid namespace, under which to register new DOIs, if specified.
  #ns = "doi:10.17910/B7"
  ## EZID credentials
  #user = "apitest"
  #pass = "apitest"
}
notification {
  ## Regex for notification emails: only emails matching this will be sent.
  filter = "*"
  ## Optional email address to copy all notification emails to (whether they pass filter or not).
  copy = "bob@nyu.edu"
}
''
