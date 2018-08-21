Documents related to building and running Databrary, copied quickly from a
collection of drafts formerly located on Confluence.

### Documentation Feedback
1.  What is the context for the instructions? When would you want to
    run them? When do you care? What is the order that you need to
    look at them in?
2.  *Motivate* the instructions
3.  "In which situation do you need to read this doc"
4.  Guidance through the forest of docs

###  Outstanding Questions/Concerns
1.  dev.databrary.org CNAME → devbrary.databrary.org →
    128.122.236.158 → canonical name: devbrary.admin.ed.nyu.edu
    1.  Has a lot of users for past employees
    2.  Required for accessing the static site; apparently unused for anything
        else.
1.  PDF email generation service

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

### Dev preview servers
#### Canonical domain names
* dev1.databrary.org
* dev8.databrary.org
#### Uses
* Preview
### Runscope target
#### Canonical domain name
apitest1.databrary.org


### Sandbox server
#### Canonical domain name
sandbox1.databrary.org





## Table of Contents

1.  [System Requirements](System-Requirements.md)
1.  Cache server
    1.  Provision
        1.  Build cache on bastion server
            ([devdatabrary2.home.nyu.edu](http://devdatabrary2.home.nyu.edu))
1.  Local Dev Environment
    1.  [Build and Run](Local-Dev-Environment/Build-and-Run.md)
    1.  Run Tests
        1.  [Run back end unit tests](Running-Tests.md)
        1.  Run functional tests locally (done - video)
    1.  Generate Reports
        1.  [Generate and publish developer documentation](Update-Github-Pages.md)
1.  Test or Review Server
    1.  Provision
        1.  Build test server
            ([apitest1.databrary.org](http://apitest1.databrary.org)) or
            review server
            ([dev1-8.databrary.org](http://dev1-8.databrary.org)) (done)
        2.  [Tear down test or review
            server](https://github.com/databrary/databrary/wiki/Tear-down-test-or-review-server)
            (draft done)
    2.  Deploy, Run
        1.  Run on test or review server (done)
    3.  Run Tests
        1.  Run back end unit tests on bastion server
            ([devdatabrary2.home.nyu.edu](http://devdatabrary2.home.nyu.edu))
            (draft done)
        2.  Run back end integration tests against test server
            ([apitest1.databrary.org](http://apitest1.databrary.org))
            (draft done)
        3.  Run functional tests against test or review server (done - video)
    4.  Monitor
        1.  Setup uptime monitoring for cache server (Bryan)
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
