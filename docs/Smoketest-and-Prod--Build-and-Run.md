## Differences between environments

Smoketest similar to prod. Differences:

  - ensure version updated on master since last deploy
  - shares remote transcoder(?) and store directories with prod
  - installation differences:
      - script for running refreshes of db from prod, applied manually
        (eventual cron job)
      - no ssl


Dev similar to smoketest. Differences:

  - empty db or restored db
  - deletes db and uploads each time branch is changed
  - installation differences:
      - transcode directory location
      - local transcoding
      - solr installed in user directory
      - location of store directories
      - no ssl
      - root directory where source code is cloned is
        /home/centos/src/databrary
## Preliminaries

All instructions below which use ssh require that your laptop be within
the NYU network or that you have connected to the VPN.

## Remote build, install

<table>
<colgroup>
<col style="width: 69%" />
<col style="width: 30%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to bastion</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu</code></pre></li>
</ul>
<p>Change to service account</p>
<ul>
<li><pre><code>sudo su - datadeploy</code></pre></li>
</ul>
<p>Get latest deploy scripts, and run deploy</p>
<ul>
<li><span style="color: rgb(3,47,98);">rm -rf ~/databrary-incubator &amp;&amp; <span style="color: rgb(3,47,98);">git clone <span class="nolink">http://github.com/databrary/databrary-incubator</span> &amp;&amp; <span style="color: rgb(3,47,98);">~/databrary-incubator/databrary-devops/deploy-stage.sh master</span></span></span></li>
</ul></td>
<td><p>Run build and copy built package to smoketest server nix-store.</p>
<p>Modify the branch if you are testing a special branch on smoketest server.</p></td>
</tr>
<tr class="even">
<td><p>From laptop, connect to bastion</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu</code></pre></li>
</ul>
<p>Change to service account</p>
<ul>
<li><pre><code>sudo su - datadeploy</code></pre></li>
</ul>
<p>Run deploy</p>
<ul>
<li><span style="color: rgb(3,47,98);"><span><span>~/databrary-incubator/databrary-devops/deploy-prod.sh </span></span></span></li>
</ul></td>
<td><p><span>Repeat for production server.</span></p>
<p><span>Can run this in parallel if desired.</span></p></td>
</tr>
</tbody>
</table>

## Manually prepare db migration script (smoketest)

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to smoketest</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu &quot;ssh -t databrary2&quot;</code></pre></li>
</ul>
<p>Create migration running script</p>
<ul>
<li><pre><code>for each desired migration (YYYYMMDD-NAME.sql), adjust and run:</code></pre>
<ul>
<li><pre><code>echo &#39;cat &lt;YYYYMMDD-NAME&gt;.sql | PGHOST=localhost psql&#39; &gt;&gt; /home/demo/runNewDbMigrations.sh</code></pre></li>
</ul></li>
</ul></td>
<td><p>Create a script to be used to run new db migrations since the last deployment. Usually this will be empty and the old &quot;runNewDbMigrations.sh&quot; script can be used and this step can be skipped.</p>
<p><br />
</p>
This step assumes that the first time that the server was created, the &quot;Reset db migration script&quot; step was run once.</td>
</tr>
</tbody>
</table>

## Connect, enter tmux session (smoketest)

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to bastion</p>
<ul>
<li><pre><code>ssh &lt;NETID&gt;@devdatabrary2.home.nyu.edu</code></pre></li>
</ul>
<p>Connect to smoketest, entering tmux session</p>
<ul>
<li><pre><code>ssh -t databrary2 &#39;sudo -u root sh -c &quot;tmux attach&quot;&#39;</code></pre></li>
</ul></td>
<td><p>You will see three windows: root, demo, log.</p></td>
</tr>
</tbody>
</table>

## Prepare for smoke test (smoketest)

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li>disconnect from VPN</li>
<li>glance at <a href="http://stage.databrary.org:443/">http://stage.databrary.org:443</a></li>
</ul></td>
<td><p>Disconnect from VPN because we have found issues in the past where using the upload feature encountered packet restrictions over NYU VPN.</p>
<p>Note: if you aren't testing uploads, you can stay connected to VPN.</p></td>
</tr>
</tbody>
</table>

<span style="font-size: 20.0px;letter-spacing: 0.0px;">Deploy
(smoketest)</span>

<table>
<colgroup>
<col style="width: 57%" />
<col style="width: 42%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li><p>on demo window, restore last night's production backup</p>
<ul>
<li>./restore</li>
</ul></li>
<li><p>on demo window, <span style="letter-spacing: 0.0px;">run any new db migrations, and start installed service</span></p>
<ul>
<li><p>bash runNewDbMigrations.sh &amp;&amp; stat -c '%y %N' databraryExeLink &amp;&amp; ./databraryExeLink</p></li>
</ul></li>
</ul></td>
<td><p>In the last step, the timestamp printed confirms build version.</p>
<p>Note: assumes smoketest is always shutdown after use.</p></td>
</tr>
</tbody>
</table>

<span style="font-size: 20.0px;letter-spacing: 0.0px;">Run smoke test
(smoketest)</span>

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li><p><span>Start browser, traverse to site (</span><a href="http://stage.databrary.org:443/" class="uri">http://stage.databrary.org:443/</a><a href="http://www.databrary.org%29%2C/">)</a></p></li>
<li><p><span>click Use Data</span></p></li>
<li><p><span>search for &quot;databrary workshop&quot;</span></p></li>
<li><p><span>open &quot;databrary sponsored workshop ..&quot; volume</span></p></li>
<li><p><span>scroll to sessions, open one</span></p></li>
<li><p><span>click on a video</span></p></li>
<li><p><span>play it</span></p></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Reset db migration script (smoketest)

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to smoketest becoming demo user</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu &quot;ssh -t databrary2&quot;</code></pre></li>
</ul>
<p>Create migration running script, erasing old</p>
<ul>
<li><pre><code>echo &#39;cd $(dirname $(readlink /home/demo/databraryExeLink))/../share/x86_64-linux-ghc-8.0.2/databrary-1/schema/&#39; &gt; /home/demo/runNewDbMigrations.sh</code></pre></li>
</ul></td>
<td><p>Can be skipped if no migrations were run.</p></td>
</tr>
</tbody>
</table>

## Stop smoketest site

<table>
<colgroup>
<col style="width: 56%" />
<col style="width: 43%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li><p>on demo window, <span style="letter-spacing: 0.0px;">kill the running service</span></p>
<ul>
<li>&lt;Ctrl-c&gt;</li>
</ul></li>
</ul></td>
<td><ul>
<li>This signals that the smoketest site is no longer in use and prevents leaving smoketest exposed.</li>
</ul></td>
</tr>
</tbody>
</table>



## Manually prepare db migration script (prod)

<table>
<colgroup>
<col style="width: 58%" />
<col style="width: 41%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to prod</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu &quot;ssh -t databrary&quot;</code></pre></li>
</ul>
<p>Create migration running script</p>
<ul>
<li><pre><code>for each desired migration (YYYYMMDD-NAME.sql), adjust and run:</code></pre>
<ul>
<li><pre><code>echo &#39;cat &lt;YYYYMMDD-NAME&gt;.sql | PGHOST=localhost psql&#39; &gt;&gt; /home/databrary/runNewDbMigrations.sh</code></pre></li>
</ul></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Connect, enter tmux session (prod)

<table>
<colgroup>
<col style="width: 58%" />
<col style="width: 41%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to bastion</p>
<ul>
<li><pre><code>ssh &lt;NETID&gt;@devdatabrary2.home.nyu.edu</code></pre></li>
</ul>
<p>Connect to prod, entering tmux session</p>
<ul>
<li><pre><code>ssh -t databrary &#39;sudo -u root sh -c &quot;tmux attach&quot;&#39;</code></pre></li>
</ul></td>
<td><p>You will see three windows: root, databrary, log.</p></td>
</tr>
</tbody>
</table>

## Prepare for smoke test (prod)

<table>
<colgroup>
<col style="width: 58%" />
<col style="width: 41%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li>glance at <a href="http://databrary.org/">http://databrary.org</a></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Await window of low activity

<table>
<colgroup>
<col style="width: 58%" />
<col style="width: 41%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li>on log window, watch for uploads</li>
</ul></td>
<td><p>This step only applies to the production server</p></td>
</tr>
</tbody>
</table>

## Deploy (prod)

<table>
<colgroup>
<col style="width: 59%" />
<col style="width: 40%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li><p>on databrary window</p>
<ul>
<li>Kill the running service<br />
&lt;Ctrl-x&gt;</li>
<li>Run any new db migrations, and start installed service<br />
bash runNewDbMigrations.sh &amp;&amp; stat -c '%y %N' databraryExeLink &amp;&amp; ./databraryExeLink</li>
</ul></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Run smoke test (prod)

<table>
<colgroup>
<col style="width: 59%" />
<col style="width: 40%" />
</colgroup>
<tbody>
<tr class="odd">
<td><ul>
<li><p>Start browser, traverse to site (<a href="https://nyu.databrary.org/" class="uri">https://nyu.databrary.org/</a><a href="http://www.databrary.org%29%2C/">)</a></p></li>
<li><p>click Use Data</p></li>
<li><p>search for &quot;databrary workshop&quot;</p></li>
<li><p>open &quot;databrary sponsored workshop ..&quot; volume</p></li>
<li><p>scroll to sessions, open one</p></li>
<li><p>click on a video</p></li>
<li><p>play it</p></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Reset db migration script (prod)

<table>
<colgroup>
<col style="width: 80%" />
<col style="width: 19%" />
</colgroup>
<tbody>
<tr class="odd">
<td><p>From laptop, connect to prod becoming databrary user</p>
<ul>
<li><pre><code>ssh -t &lt;NETID&gt;@devdatabrary2.home.nyu.edu &quot;ssh -t databrary&quot;</code></pre></li>
</ul>
<p>Create migration running script, erasing old</p>
<ul>
<li><pre><code>echo &#39;cd $(dirname $(readlink /home/databrary/databraryExeLink))/../share/x86_64-linux-ghc-8.0.2/databrary-1/schema/&#39; &gt; /home/databrary/runNewDbMigrations.sh</code></pre></li>
</ul></td>
<td><p><br />
</p></td>
</tr>
</tbody>
</table>

## Notes and Questions:

  - rollback plan for complex deploys should be to prepare a branch with
    the changes ready, including any extra db migrations to undo, in a
    prepared PR 
  - example ssh + sudo: 
      - ssh -t  \<NETID\>@smoketest "sudo -i -u build sh -c
        '/home/build/databrary/stage-pull-migrate-build.sh'"   \# -t to
        enter password for sudo; sh -c to be able to enter a sequence of
        commands
  - watch terminal for output for now, eventually Jenkins will record
    log output
  - periodically, run the following to clear out stale disconnected tmux
    clients
      - tmux detach-client -a
  - often, solr will fail to start because the prior solr kept running.
    I am not sure why the prior solr is not stopped properly. This is
    mostly ok, because the main thing is that solr is still, in fact,
    running
