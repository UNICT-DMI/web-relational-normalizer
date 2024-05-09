FROM mitmers/sbcl:alpine

EXPOSE 8080
USER root

RUN apk add curl

WORKDIR /root/

RUN curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
    --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
    --eval '(ql:add-to-init-file)' \
    --quit

COPY fundep/ .quicklisp/local-projects/fundep/
COPY newnormalizer/ .quicklisp/local-projects/newnormalizer/
COPY main .

RUN sbcl \
    --eval '(load "~/.quicklisp/setup.lisp")' \
    --eval '(ql:quickload "alexandria")' \
    --eval '(ql:quickload "dlist")' \
    --eval '(ql:quickload "hunchentoot")' \
    --eval '(ql:quickload "ht-simple-ajax")' \
    --eval '(ql:quickload "cl-who")' \
    --quit

ENTRYPOINT [ "./main" ]