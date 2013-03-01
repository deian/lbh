## About

LearnByHacking is a web application that allows users to write tutorials, blogs, lectures, etc. by incorpoating active code. That is, code that end-users can execute with a single click, without installing any software on their machine. This app was inspired by School of Haskell, but differs form SoH in several ways. Firstly, it is more light weight--using process-level isolation vs. creating VMs; secondly it supports languages beyond Haskell (currently it allows users to implement active C, C++, Javascript, and Haskell code); thirdly, it allows users to collaborate on content creation, i.e., post writing; fourthly, it is open source.

LbH is implement using the [Hails](https://github.com/scslab/hails) web-framework, which follows the Mode-Policy-View-Controller model.  Hails allows us to minimize the amount of trusted code. Specifically, our controllers and views are not considered _trusted_ and thus not relied upon to conduct security-critical operations. What does this mean? Well, it means we can extend LbH with third-party developers in a much shorter amount of time. So, hack, and send us pull-requests!

## Install

LbH uses Hails' OS-level confinement mechanism to execute arbitrary code, which currently relies on Arch Linux. If you are developing on a different distribution, please let us know and we can make a development version of [lio-cjail](https://github.com/scslab/cjail), the Haskell package that communicates with our jail tool, that executes processes without isolation. (We could also be convinced to port this to other distros, depending on demand.)

For the rest of this section I will assume that you are installing LbH on Arch. Except for the jail related stuff, things should work analogously on other distros/OSes.

### Installing required tools

Hails depends on MongoDB and Haskell. As usual, you can install these tools with pacman:

```bash
# Hails depends on MongoDB:
sudo pacman -S mongodb

# Start MongoDB:
rc.d start mongodb

# Hails depends on the GHC Haskell compiler:
sudo pacman -S ghc

# It's useful to have cabal, the Haskell "package manager": 
sudo pacman -S cabal-install
```

### Creating new user

To avoid pwning your system in case of unknown vulnerabilities, let's create a new UID that we'll use to run LbH:

```bash
sudo useradd -m -d /disk/hs08/lbh -s /bin/bash lbh
```

As user lbh, let's also install cabal-dev, as to avoid dependency nightmares on your system:

```bash
cabal update
cabal install cabal-dev
export PATH=$PATH:$HOME/.cabal/bin
```

### Grab sources

LbH depends on the Arch-specific jail tool we implemented. Hence, we must install this from source:

```bash
# Grab the LbH source:
git clone git://github.com/deian/lbh.git
# Grab the Arch jail tool and Haskell bindings:
git clone git://github.com/scslab/cjail.git
# Grab the Hails wrappers for the bindings:
git clone git://github.com/scslab/lio-cjail.git
```

### Installing tools and packages

Let's first install the Arch jail system:

```bash
# As lbh:
pushd cjail
make
# As root:
make install
popd
```

Let's now install all the Haskell dependencies and the LbH app:

```bash
cd lbh
# Install cjail:
cabal-dev install ../cjail/cjail-haskell/
# Install Hails cjail wrappers:
cabal-dev install ../lio-cjail
# Install (including dependencies)
cabal-dev install
# Add Hails to the path
export PATH=$PATH:./cabal-dev/bin
```

This installed LbH and you can directly run it, but you won't be able to execute arbitrary code. Nevertheless, let's run it without this feature:

```bash
hails -a LBH.Controllers -s ./cabal-dev --unsafe --persona-audience http://localhost:8080
```

Point your browser to [http://localhost:8080/](http://localhost:8080/) to start playing.

The `--unsafe` flag indicates that there is some trusted code in this app and thus cannot be considered _safe_ (see [Safe Haskell](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/safe-haskell.html) for a discussion of what safe means).

### Creating the jail

Let's first create a new jail in directory `lbh-jail`. As root:

```bash
mkcjail -c lbh-jail.conf lbh-jail
```

The configuration file specifies which Arch packages should be installed. In this case we're installing ghc, js, and clang which will be used by our app to execute Haskell, JavaScript, and C/C++ programs, respectively.

Depending on your version of pacman, the install may fail because Yaourt keys aren't signed, so you may want to modify `./lbh-jail/root/etc/pacman.conf` to uncomment `SigLevel = Optional TrustAll` to continue.

As lbh, you can now test out the jail:

```bash
cjail lbh-jail bash
```

### Adding active-code binary to jail

When you installed LbH with cabal you installed a binary called `activeCode` in `./cabal-dev/bin`. This binary is a wrapper that LbH calls with code it wishes to execute. Since LbH can only make calls to jailed processes, let's copy the program.

As root:

```bash
cp ./cabal-dev/bin/activeCode ./lbh-jail/root/usr/bin
```

Now that we have everything in place, let's tell Hails about the jail.

```bash
export LIO_CJAIL_DIR=./lbh-jail
```

## Running LbH

You can run the application in develoopment mode or production.

### Development

The simplest way to run the app in development mode was already mentioned above. Simply use:

```bash
hails -a LBH.Controllers -s ./cabal-dev --unsafe --persona-audience http://localhost:8080
```

That's all, create posts and have fun!

### Production

In production mode you should use an environment file. Create `prod.env`:

```bash
APP_NAME=LBH.Controllers
PORT=8080
PERSONA_AUDIENCE=http://www.your-domain.org
HMAC_KEY=your-secrect-very-long-key
DATABASE_CONFIG_FILE=database.conf
HAILS_MONGODB_SERVER=localhost
PACKAGE_CONF=./cabal-dev/packages-7.6.2.conf
LIO_CJAIL_DIR=./lbh-jail
LIO_CJAIL_TIMEOUT=1
```

Now you can run the app with:

```bash
hails --env=prod.env --unsafe
```
Again, the `--unsafe` is necessary becasuse LbH does not treat email addresses as username and so some trusted code is necessary to translate between the two.

Finally, as root, let's install nginx as a front-end:

```bash
pacman -S nginx
```

and route port 80 traffic to 8080 by modify `/etc/nginx/nginx.conf` to add:

```
server {
     listen         80;
     server_name    your-domain.org;
     rewrite ^(.*) http://www.$host$1 permanent;
   }

   server {
       listen       80;
       server_name  www.your-domain.org;

       error_page   500 502 503 504  /50x.html;
       location = /50x.html {
           root   html;
       }

       location / {
           proxy_pass   http://127.0.0.1:8080;
           proxy_set_header Host $host;
       }

   }
```

## Status

LbH is still very much in (pre-)beta stages. As such, we hope to integrate additional features (e.g., a commenting system, full-text search, RSS feeds, etc.), and address certain limitations (e.g., we currently do not protect against CSRF).
