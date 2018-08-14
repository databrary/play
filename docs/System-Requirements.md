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
