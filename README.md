haskchan
haskchan is the pedagogical Haskell imageboard (I made it to teach myself Haskell).
Requirements
A Linux (or BSD) server — these instructions assume Debian/Ubuntu
Stack (handles GHC and all Haskell dependencies)
`sqlite3` (for admin user setup)
~2GB of free memory and ~1 hour of build time for the first build
Quick start
```bash
# 1. Clone the repo
git clone https://github.com/konamicode01/haskchan.git haskchan
cd haskchan

# 2. Install non-Haskell build dependencies (Debian/Ubuntu)
sudo apt update
sudo apt install -y pkg-config libavutil-dev libavformat-dev libavdevice-dev \
    libavcodec-dev libswscale-dev zlib1g-dev

# 3. Install Stack, if you don't already have it
curl -sSL https://get.haskellstack.org/ | sh

# 4. Build (downloads GHC + all dependencies, ~1 hour, ~2GB RAM)
stack build

# 5. Create the server secret (used for login cookies and tripcodes, min 32 bytes)
dd if=/dev/urandom of=secret.bin count=1 bs=32

# 6. Run
stack run
```
By default this starts haskchan on `localhost:7000`.
Configuring
Site configuration lives in `app/Main.hs`. Before running, check that these paths are correct for your setup:
database file — SQLite db path
secret file — path to the `secret.bin` you created above; must exist and be ≥32 bytes
static folder — created automatically if missing
captcha folder — created automatically if missing
font file — used to render captcha images; defaults to `DejaVuSansMono-Bold.ttf`. If you don't have this font installed, point it at a font file that exists on your system (`fc-list | grep -i dejavu` to check, or `apt install fonts-dejavu`)
Make sure the user running `stack run` has write permission to the db file, static folder, and captcha folder.
Creating an admin user
There's no built-in admin-creation command yet, so:
Register a normal account on the running site.
Promote it to admin directly in the database:
```bash
sqlite3 haskchan.db
sqlite> UPDATE user SET admin = 1;
```
⚠️ This promotes every existing user to admin — fine for a fresh install with one account, but don't run it once you have real users. If you need to target a single user, adjust the query with a `WHERE` clause matching your `user` table's username/id column.
Running on a remote server (e.g. via SSH)
If you're deploying to a VPS:
```bash
ssh root@your-server
cd ~/haskchan
git pull            # or clone as above on first setup
stack build
stack run
```
`stack run` runs in the foreground on port 7000. For a real deployment you'll want to:
Run it under a process supervisor (systemd service, `screen`/`tmux`, or a tool like `supervisord`) so it survives disconnecting your SSH session
Put a reverse proxy (nginx/Caddy) in front of it for TLS and to expose it on port 80/443
Run it as a non-root user rather than `root`, for security
Example minimal systemd unit (`/etc/systemd/system/haskchan.service`):
```ini
[Unit]
Description=haskchan imageboard
After=network.target

[Service]
WorkingDirectory=/home/haskchan/haskchan
ExecStart=/home/haskchan/haskchan/.stack-work/install/.../bin/haskchan-exe
Restart=on-failure
User=haskchan

[Install]
WantedBy=multi-user.target
```
(Adjust `ExecStart` to the actual binary path Stack produces — run `stack path --local-install-root` to find it, or just use `stack exec haskchan-exe` if the entry point is named differently.)
Proxying Stack through Tor (optional)
Stack only accepts HTTP proxies, so it can't proxy through Tor directly. You can bridge an HTTP proxy to a SOCKS proxy with Privoxy (Polipo is abandoned, so use Privoxy instead):
```bash
apt install privoxy
editor /etc/privoxy/config
```
Uncomment this line in the config:
```
forward-socks5t / 127.0.0.1:9050 .
```
Then set the proxy environment variables before running Stack commands:
```bash
export HTTP_PROXY=http://127.0.0.1:8118
export HTTPS_PROXY=http://127.0.0.1:8118
```
Privoxy listens on port 8118 and will forward through Tor on 9050.
Troubleshooting
"file not found" on `stack run` — you haven't created the secret file yet; see step 5 above.
Build fails on `ffmpeg-light` — you're missing the non-Haskell dependencies; see step 2 above.
Captcha images fail to generate — the configured font file doesn't exist on disk; update the font path in `app/Main.hs`.
