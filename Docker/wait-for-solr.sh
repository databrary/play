#!/bin/bash
# wait-for-postgres.sh

set -e

host="$1"
shift
cmd="$@"

until curl http://$host:8983/solr/admin/cores?action=STATUS; do
  >&2 echo "Solr is unavailable - sleeping"
  sleep 5
done

>&2 echo "Solr is up - executing command"
exec $cmd

