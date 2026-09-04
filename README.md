## phi

phi is the pedagogical Haskell imageboard (I made it to teach myself Haskell).

### Setup

#### Install Stack

A tool called Stack handles dependencies and the build process, and you need it to build phi. https://docs.haskellstack.org/en/stable/README/#how-to-install

##### Proxying Stack through Tor

Stack only accepts HTTP proxies, so it cannot proxy through Tor directly. It is possible to create an HTTP proxy that redirects to a SOCKS proxy, for example with Polipo or Privoxy. Polipo is abandoned so to get this to work with Privoxy you need to install it and uncomment the Tor example lines in `/etc/privoxy/config`. If Privoxy is running you should have an HTTP proxy at port 8118 that will go through Tor.

For example on Debian:

Install Privoxy
```
apt install privoxy
editor /etc/privoxy/config
```

Uncomment this line in the config
```
forward-socks5t / 127.0.0.1:9050 .
```

Set http proxy environment variables
```
export HTTP_PROXY=http://127.0.0.1:8118
export HTTPS_PROXY=http://127.0.0.1:8118
```

Now Stack will proxy through Tor.

#### Non-Haskell dependencies

Some of the dependencies e.g. `ffmpeg-light` require non-Haskell packages. These are the names of the required packages in Debian. It should be similar on other Linux distributions and BSD.

```pkg-config libavutil-dev libavformat-dev libavdevice-dev libavcodec-dev libswscale-dev zlib1g-dev```

#### Building

Run `stack build` build phi. Before that happens it will download GHC (the compiler) and compile all the dependencies. This will take on the order of 1 hour and 2GB of memory.
Run `stack run` to start phi on localhost port 7000.

#### Configuring

If you ran `stack run` as above you will have gotten a "file not found" error. This is because the server secret file does not exist yet. Take a look at the site configuration in `app/Main.hs`, it specifies these things:
- database file
- secret file
- static folder
- captcha folder
- font file

The secret file is used for verifying login cookies and generting secure tripcodes. It needs to exist. It should be at least 32 bytes. It could be larger but it isn't necessary. On Linux you can create it with `dd`:

```dd if=/dev/urandom of=secret.bin count=1 bs=32```

The font file is used for generating captcha images. By default it points to `DejaVuSansMono-Bold.ttf` but you may not have this font, make sure it points to a font that exists.

The remaining files (db file, static folder, captcha folder) will be created if they don't exist already, just make sure the running user has write permissions for all of them.

#### Creating an admin user

**TODO**

As of now to create an admin user you can register on the site but then you have to edit the database manually to promote that user to admin.

Using `sqlite3`:

```
$ sqlite3
sqlite> .open phi.db
sqlite> UPDATE user SET admin = 1;
```

This will make ALL existing users admin so just make sure you know that.
