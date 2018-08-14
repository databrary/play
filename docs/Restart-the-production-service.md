# Preliminaries
All instructions below which use ssh require that your laptop be within the NYU network or that you have connected to the VPN.

# Connect, enter tmux session
From laptop, connect to bastion
```
ssh <NETID>@devdatabrary2.home.nyu.edu
```
Connect to prod, entering tmux session
```
ssh -t databrary 'sudo -u root sh -c "tmux attach"'
```
You will see three windows: root, databrary, logs.

# Await window of low activity
on log window, watch for uploads

# Restart
on databrary window:

Kill the running service
```
<Ctrl-x>
```
Run empty db migrations script, and start installed service
```
bash runNewDbMigrations.sh && stat -c '%y %N' databraryExeLink && ./databraryExeLink
```

# Notes:
* As noted in deployment instructions, the application often fails to shutdown solr properly. If that happens, when the databrary service starts up again, solr will fail to start. The error is benign, as the prior instance will still be running.
* postgresql is controlled by systemd
  * The service configuration file is /usr/lib/systemd/system/postgresql-9.5.service
  * There should be no need to manually restart postgresql. If truly desired, one can use systemd standard commands to control the service.
* nginx is controlled by systemd
  * The service configuration file is /usr/lib/systemd/system/nginx.service
  * There should be no need to manually restart nginx. If truly desired, one can use systemd standard commands to control the service.
* tmux appears to be manually controlled
  * after a system restart, one probably needs to login as root and run "tmux attach", which will trigger starting tmux server, and starting a session and windows as specified in /root/.tmux.conf
  * in /root/.tmux.conf, there are steps to create a window for "databrary" and "logs"
* All these instructions also apply to deployment rehearsal/failover server (databrary2). Replace user databrary with user demo on deployment rehearsal.