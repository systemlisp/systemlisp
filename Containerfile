FROM ubuntu

MAINTAINER Stefan T. Petre

RUN apt-get update
RUN apt-get install -y sbcl
RUN apt-get install -y curl
RUN apt-get install -y git
RUN apt-get install -y sqlite3
RUN apt-get install -y gtkwave
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN touch $HOME/.sbclrc
RUN printf "\n" | sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
RUN cat $HOME/.sbclrc
RUN cd $HOME/quicklisp/local-projects && git clone https://github.com/systemlisp/systemlisp.git
RUN sbcl --eval '(ql:quickload "system-lisp")'

