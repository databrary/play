FROM postgres
ENV POSTGRES_PASSWORD mysecretpassword
RUN apt-get update
RUN apt-get install -y libpq-dev postgresql-server-dev-all git gcc
WORKDIR /usr/local/src
RUN git clone git://github.com/databrary/databrary
WORKDIR databrary
ARG commit=origin/build_system
RUN git remote update && git checkout $commit
RUN make -C pgranges install

ADD init-user-db.sh /usr/local/src/databrary/init-user-db.sh
RUN chmod 755 /usr/local/src/databrary/init-user-db.sh
ADD remove-superuser-db.sh /usr/local/src/databrary/remove-superuser-db.sh
RUN chmod 755 /usr/local/src/databrary/remove-superuser-db.sh
