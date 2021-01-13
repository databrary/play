#!/usr/bin/env bash
# Control interface for transcode jobs.
# This is run directly from the application, on the webserver.
# It calls transcode, possibly on transcode.host.

escape() {
	for a in "$@" ; do
		echo \'${a//\'/\'\\\'\'}\'
	done
}

warn () {
	>&2 echo "$@"
}

cmd=`dirname $0`/transcode

if [[ ! -f $cmd ]] ; then
	echo "$cmd: not found" >&2
	exit 2
fi

while getopts 'i:h:d:v:m:c:k:s:r:f:t' opt ; do case "$opt" in
	i) id=$OPTARG ;;
	h) host=$OPTARG ;;
	d) dir=$OPTARG ;;
	v) version=$OPTARG ;;
	m) mount=$OPTARG ;;

	c) collect=$OPTARG ;;
	k) kill=$OPTARG ;;
	s) src=$OPTARG ;;
	r) url=$OPTARG ;;
	f) fmt=$OPTARG ;;
	t) test=1 ;;

	?) exit 1 ;;
esac ; done

hcmd=./transcode${version:+-$version}

if [[ -n $test ]] ; then
	if [[ -z $dir ]] ; then
                # NOT REACHED: getopts will have already thrown an error.
		false
	elif [[ -n $host ]] ; then
		echo "ssh into $host"
		ssh "$host" test -d "$dir"
		if [[ -n $mount ]] ; then
			echo "Copy $cmd to $mount/$hcmd"
			cp -pf "$cmd" "$mount/$hcmd"
			ssh "$host" rsync -p "$mount/$hcmd" "$hcmd"
		else
			echo "rsync $cmd to $host:$hcmd"
			rsync -p "$cmd" "$host:$hcmd"
		fi
	else
		if ! test -d "$dir"
                then
                    warn "$0: $dir does not exist"
                    false
                fi
	fi
	exit $?
fi

if [[ -z $id || -z $dir || -z $collect$kill && ( -z $src || -z $url || -z $fmt ) ]] ; then
	echo "$0: usage error: $*" >&2
	exit 1
fi

if [[ -n $collect ]] ; then
	echo "Collect $collect"
	if [[ -n $host ]] ; then
		echo "Collect from host $host"
		if [[ -n $mount ]] ; then
			echo "Collect from $dir/$id.$fmt to $mount/$dir/$id.$fmt"
			ssh "$host" rsync "$dir/$id.$fmt" "$mount/$dir/$id.$fmt"
			mv "$mount/$dir/$id.$fmt" "$collect"
			rm -f "$mount/$dir/$id"
		else
			echo "Collect from $host:$dir/$id.$fmt to $collect"
			rsync "$host:$dir/$id.$fmt" "$collect"
		fi
		echo "Collect ssh and remove $dir/$id and $dir/$id.$fmt"
		ssh "$host" rm -f "$dir/$id" "$dir/$id.$fmt"
	else
		mv "$dir/$id.$fmt" "$collect"
		rm -f "$dir/$id"
	fi
elif [[ -n $host ]] ; then
	if [[ -z $kill ]] ; then
		echo "host $host"
		if [[ -n $mount ]] ; then
			echo "ln $src to $mount/$dir/$id"
			ln -fT "$src" "$mount/$dir/$id"
		else
			echo "rsync $src to $host:$dir/$id"
			rsync "$src" "$host:$dir/$id"
		fi
	fi
	echo "run $hcmd in $host"
	# grab only job id from e.g. "Submitted batch job 234324324"
	ssh "$host" "$hcmd" `escape "$@"` | sed 's/^[^0-9]*\([0-9]\+\)$/\1/'
elif [[ -n $kill ]] ; then
	"$cmd" "$@"
else
	ln -fT "$src" "$dir/$id"
	"$cmd" "$@" &
	echo $!
fi

