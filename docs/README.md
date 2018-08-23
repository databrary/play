# Databrary wind-down

While this document will eventually be the home page for documentation about
running the Databrary service, currently it also the project page for winding
down Databrary at the end of August, 2018.

## Tasks

* [ ] Dump relevant docs from Confluence into this directory
* [ ] Back up the sandbox database
* [ ] Shut down dev2.databrary.org, dev8.databrary.org, and any other
  devXX.databrary.org servers
* [ ] Shut down sandbox1.databrary.org
* [ ]

## Scratch Space

### Documentation Feedback
1. What is the context for the instructions? When would you want to
   run them? When do you care? What is the order that you need to
   look at them in?
2. *Motivate* the instructions
3. "In which situation do you need to read this doc"
4. Guidance through the forest of docs

###  Outstanding Questions/Concerns
1. dev.databrary.org CNAME → devbrary.databrary.org →
    128.122.236.158 → canonical name: devbrary.admin.ed.nyu.edu
    1. Has a lot of users for past employees
    2. Required for accessing the static site; apparently unused for anything
       else.
1. What is https://github.com/databrary/curation? Waiting on news from Joy.
1. Clean up Github organization
    1. Remove all teams?
    1. Remove private repos with sensitive data?
    1. Set member permissions to 'write'?
       https://github.com/organizations/databrary/settings/member_privileges
    1. Split Datavyu into its own organization?

-------

## Systems

### Bastion server
#### Canonical domain name
devdatabrary2.home.nyu.edu
#### Uses
* Bastion (access point for other systems on the NYU network, accessible to
  anyone on the VPN)
* Project-specific Nix cache. With it, an initial build takes minutes, instead
  of tens of hours.

### App server
#### Canonical domain name
databrary2.home.nyu.edu
#### Uses
* Runs the Databrary application


## Auxiliary services

1. Generating access agreements: https://github.com/databrary/policies/
1. Generating static website: https://github.com/databrary/www/

## Databrary web service operations

1.  [System Requirements](System-Requirements.md)
1.  Cache server
    1.  Provision
        1.  Build cache on bastion server
            ([devdatabrary2.home.nyu.edu](http://devdatabrary2.home.nyu.edu))
    1.  Monitor
        1.  Setup uptime monitoring for cache server (Bryan)
1.  Local Dev Environment
    1.  [Build and Run](Local-Dev-Environment/Build-and-Run.md)
    1.  Run Tests
        1.  [Run back end unit tests](Running-Tests.md)
        1.  Run functional tests locally (done - video)
    1.  Generate Reports
        1.  [Generate and publish developer documentation](Update-Github-Pages.md)
1.  Sandbox (sandbox1.databrary.org)
    1.  Provision
        1.  Build demo server (done)
        2.  Tear down demo server (draft done)
    2.  Deploy, Run
        1.  Run on demo server (done)
1. Smoketest (databrary2.home.nyu.edu) (aka Stage or Staging)
    1.  [Deploy, Run](Smoketest-and-Prod--Build-and-Run.md)
    2.  Run Tests
        1.  Run functional tests against smoketest server (done - video)
1. Production Static Web Content
    1.  Publish to static server (Bryan)
1. Production Web Service (databrary.home.nyu.edu)
    1.  Deploy, Run
        1.  Build and deploy to production server (done)
    2.  Monitor
        1.  Setup uptime monitoring (Bryan)
    3.  Recover
        1.  [Restart the production service](Restart-the-production-service.md)
        2.  Manually start the service after production machine reboot (Kanishka)
        3.  [Manually resubmit transcoding failures after scheduled
            transcoding cluster
            outage](https://github.com/databrary/databrary-incubator/blob/master/prototype-reports/clusterOutageJobs.sql)
            (draft done)
        4.  Repair transcoding node connectivity if compute cluster
            changes ssh public key (draft done)
