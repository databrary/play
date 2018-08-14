Purpose: List all the documents we have about running Databrary web
service operations, make them easier to discover

### Definitions
1.  hpc
2.  smoketest
3.  ezid
### Feedback
1.  What is the context for the instructions? When would you want to
    run them? When do you care? What is the order that you need to
    look at them in?
2.  *Motivate* the instructions
3.  "In which situation do you need to read this doc"
4.  Guidance through the forest of docs
###  Outstanding Questions
1.  dev.databrary.org CNAME → devbrary.databrary.org →
    128.122.236.158 → canonical name: devbrary.admin.ed.nyu.edu
    1.  Has a lot of users for past employees

-------

## Table of Contents

4.  [System Requirements](System-Requirements.md)
5.  TODO Auxiliary things?
    1.  PDF email generation service
6.  Cache server
    1.  Provision
        1.  Build cache on bastion server
            ([devdatabrary2.home.nyu.edu](http://devdatabrary2.home.nyu.edu))
7.  Local Dev Environment
    1.  Provision
        1.  [Build development environment](Local-Dev-Environment/Build.md)
        2.  Tear down development environment (Kanishka)
    2.  Deploy,
            Run
        1.  [Run](Local-Dev-Environment/Build-and-Run.md)
    3.  Run Tests
        1.  [Run back end unit tests](Running-Tests.md)
        2.  Run smoke test (done)
        3.  Run functional tests locally (done - video)
        4.  Exercise local development automation (partially done)
    4.  Generate Reports
        1.  [Generate and publish developer documentation](Update-Github-Pages.md)
8.  Test or Review Server
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
        3.  Run functional tests against test or review server (done -
            video)
    4.  Monitor
        1.  Setup uptime monitoring for cache server (Bryan)
9.  Sandbox (sandbox1.databrary.org)
    1.  Provision
        1.  Build demo server (done)
        2.  Tear down demo server (draft done)
    2.  Deploy, Run
        1.  Run on demo server (done)
10. Smoketest (databrary2.home.nyu.edu)
    1.  Deploy, Run
        1.  Build and deploy to smoketest server (done)
    2.  Run Tests
        1.  Run functional tests against smoketest server (done - video)
11. Production Static Web Content
    1.  Publish to static server (Bryan)
12. Production Web Service (databrary.home.nyu.edu)
    1.  Deploy, Run
        1.  Build and deploy to production server (done)
    2.  Monitor
        1.  Setup uptime monitoring (Bryan)
    3.  Recover
        1.  [Restart the production service](Restart-the-production-service.md)
        2.  Manually start the service after production machine reboot
            (Kanishka)
        3.  [Manually resubmit transcoding failures after scheduled
            transcoding cluster
            outage](https://github.com/databrary/databrary-incubator/blob/master/prototype-reports/clusterOutageJobs.sql)
            (draft done)
        4.  Repair transcoding node connectivity if compute cluster
            changes ssh public key (draft done)

In progress guides for a possible future version of this page.
