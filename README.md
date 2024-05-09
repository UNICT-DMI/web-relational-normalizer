# Web Relational Normalizer
As the original tool developed by [Renzo Orsini](https://iris.unive.it/cris/rp/rp03752) from [Università Ca' Foscari Venezia](https://unive.it) is not available anymore, here's the version available from their [official research catalogue](https://iris.unive.it/handle/10278/3687995).
This tool is one of the suggested tool to check exercises during Database course @ [DMI UniCT](https://web.dmi.unict.it).
The code has little changes in order to resolve some issues during the startup phase.

## 🚀 Getting started
There are two ways to run this repository: Docker or local installation.
> [!NOTE]
> Project's default port is 8080.

### 🐳 Docker
The easiest is to use the image published in this repository or the Dockerfile available.
In order to install the normalizer with the provided Dockerfile you need to clone this repository and `cd` into the cloned repo.
```
docker build --tag 'relational-normalizer' .
docker run -d --name  'web-relational-normalizer' 'relational-normalizer'
```

### 💻 Local installation
Install [sbcl](http://www.sbcl.org/getting.html) from source.
> [!WARNING]
> There are also versions available from many packages manager, but sometimes they could create compatibility problems.
> Try at your own risk!

Now it's [QuickLisp](https://www.quicklisp.org/beta/#installation)'s turn, make sure to install cURL or to download manually the following file
```
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
    --eval '(ql:add-to-init-file)' \
    --quit
```

Now let's move our project into quicklisp's local-project
```
mv fundep/ ~/.quicklisp/local-projects/fundep/
mv newnormalizer/ ~/.quicklisp/local-projects/newnormalizer/
```

And finally install the project's dependencies
```
sbcl \
    --eval '(load "~/.quicklisp/setup.lisp")' \
    --eval '(ql:quickload "alexandria")' \
    --eval '(ql:quickload "dlist")' \
    --eval '(ql:quickload "hunchentoot")' \
    --eval '(ql:quickload "ht-simple-ajax")' \
    --eval '(ql:quickload "cl-who")' \
    --quit
```

Finally, you can run it with
```
sbcl --load ~/.quicklisp/setup.lisp \
    --eval 'ql:quickload "newnormalizer"'
    --eval 'NN:start-web-server T'
    --eval 'sleep #xffffffff'
```
