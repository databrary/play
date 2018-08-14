Purpose: List all the documents we have about running Databrary web
service operations, make them easier to discover



1.  Definitions
    1.  hpc
    2.  smoketest
    3.  ezid
2.  Feedback
    1.  What is the context for the instructions? When would you want to
        run them? When do you care? What is the order that you need to
        look at them in?
    2.  *Motivate* the instructions
    3.  "In which situation do you need to read this doc"
    4.  Guidance through the forest of docs
3.  Outstanding Questions
    1.  dev.databrary.org CNAME → devbrary.databrary.org →
        128.122.236.158 → canonical name: devbrary.admin.ed.nyu.edu
        1.  Has a lot of users for past employees
4.  System Requirements
    1.  HPC cluster with Slurm
        1.  ssh key for databrary user for access
    2.  EZID account
    3.  Server for static web content and PDF email generation
        1.  dev.databrary.org bastion to access it
    4.  NYU NFS mount / disk storage (**may be Databrary-specific within
        NYU?**)
        1.  Temp space for upload procedure
        2.  Permanent storage of researcher's primary-source video data
        3.  Pass files between web node and transcoding node
        4.  Permanent place for database backups
    5.  Users
        1.  On Prod, Cache, and Smoketest servers
            1.  databrary and datadeploy
        2.  databrary@Cache public key goes everywhere (Cache serves as
            bastion host)
    6.  Email
        1.  Allow web server relay access to GSuite email
        2.  Accounts @databrary.org (**Where do these get
            forwarded?**)
            1.  authorize@
            2.  curation@
            3.  help@
    7.  NYU-controlled firewall
        1.  0.0.0.0 access to 80, 443 for all .home.nyu.edu systems
        2.  443 gets forwarded to 4433 *in the firewall* for unknown
            reasons\\
        3.  0.0.0.0 access to 5000 on Cache (for nix cache)
    8.  nginx forwards from 80 to 443 on Prod and Smoketest
    9.  TLS certs (provenance unknown)
    10. Solr running on Prod and Smoketest (seems to have been installed
        by Dylan? And is managed by the web service, not by the system)
    11. DNS entries
        1.  Linode: databrary.org
            1.  nyu
            2.  [www](http://www.databrary.org)
            3.  stage (legacy name for smoketest1)
            4.  databrary1 (alias for www?)
        2.  NYU: home.nyu.edu
            1.  devdatabrary2
            2.  databrary
            3.  databrary2
5.  TODO Auxiliary things?
    1.  PDF email generation service
6.  Cache server
    1.  Provision
        1.  Build cache on bastion server
            ([devdatabrary2.home.nyu.edu](http://devdatabrary2.home.nyu.edu))
7.  Local Dev Environment
    1.  Provision
        1.  [Build development
            environment](https://github.com/databrary/databrary/blob/39de1752ecffae20db926a6492ece132a3c14e2b/docs/Local-Dev-Environment/Build.md)
        2.  Tear down development environment (Kanishka)
    2.  Deploy,
            Run
        1.  <span>[Run](https://github.com/databrary/databrary/blob/a2425eff038417b7963bb7392a52320a1c827cf3/docs/Local-Dev-Environment/Build-and-Run.md)</span>
    3.  Run Tests
        1.  [Run back end unit
            tests](https://github.com/databrary/databrary/blob/bfd788e14c948af9c648a443a2026ab168e0ab59/docs/Running-Tests.md)
        2.  Run smoke test (done)
        3.  Run functional tests locally (done - video)
        4.  Exercise local development automation (partially done)
    4.  Generate Reports
        1.  [Generate and publish developer
            documentation](https://github.com/databrary/databrary/blob/5ac040d3f5b340f0fd01a2dcdbeb420c1f5d8ecf/docs/Update-Github-Pages.md)
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
        1.  [Restart the production
            service](https://github.com/databrary/databrary/wiki/Restart-the-production-service)
            (done)
        2.  Manually start the service after production machine reboot
            (Kanishka)
        3.  [Manually resubmit transcoding failures after scheduled
            transcoding cluster
            outage](https://github.com/databrary/databrary-incubator/blob/master/prototype-reports/clusterOutageJobs.sql)
            (draft done)
        4.  Repair transcoding node connectivity if compute cluster
            changes ssh public key (draft done)

In progress guides for a possible future version of this page.
