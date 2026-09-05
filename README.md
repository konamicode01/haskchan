# Haskchan

**A lightweight, self-hosted imageboard written in Haskell.**

Simple to deploy. Easy to modify. Built for speed.

[![Language](https://img.shields.io/badge/language-Haskell-5e5086?logo=haskell&logoColor=white)](https://www.haskell.org/)
[![Build Tool](https://img.shields.io/badge/build-Stack-3776AB)](https://docs.haskellstack.org/)
[![GHC](https://img.shields.io/badge/GHC-8.10.7-9370DB)](https://www.haskell.org/ghc/)
[![Web Server](https://img.shields.io/badge/server-Warp-orange)](https://hackage.haskell.org/package/warp)
[![Database](https://img.shields.io/badge/database-SQLite-003B57?logo=sqlite&logoColor=white)](https://www.sqlite.org/)
[![Platform](https://img.shields.io/badge/platform-Linux-FCC624?logo=linux&logoColor=black)](#requirements)
[![Tor](https://img.shields.io/badge/Tor-onion--service%20ready-7D4698?logo=torproject&logoColor=white)](#tor-onion-service)
[![License](https://img.shields.io/badge/license-see%20repo-lightgrey)](#license)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen)](#contributing)

`haskell` · `imageboard` · `chan` · `warp` · `sqlite` · `self-hosted` · `ffmpeg` · `imagemagick` · `tor` · `onion-service` · `captcha` · `image-upload` · `webm` · `mp4` · `pdf-upload` · `moderation`

[Website](https://haskchan.me) · [Source](https://github.com/konamicode01/haskchan) · [Report a Bug](#reporting-bugs)

</div>

---

## Table of Contents

- [Features](#features)
- [Media Support](#media-support)
- [Thumbnail Processing](#thumbnail-processing)
- [Requirements](#requirements)
- [Installation](#installation)
- [Authentication and Security](#authentication-and-security)
- [CAPTCHA](#captcha)
- [Static File and MIME Type Support](#static-file-and-mime-type-support)
- [Configuration](#configuration)
- [Production Deployment](#production-deployment)
- [Tor Onion Service](#tor-onion-service)
- [Updating Haskchan](#updating-haskchan)
- [Development](#development)
- [Contributing](#contributing)
- [Reporting Bugs](#reporting-bugs)
- [License](#license)

---

## Features

| Category | Details |
|---|---|
| **Core** | Imageboard-style threads and replies |
| **Uploads** | Images, PDFs, WebM/MP4/MOV/M4V, and audio |
| **Thumbnails** | Automatic WebP and JPEG generation |
| **Anti-spam** | CAPTCHA support with click-to-refresh |
| **Accounts** | User registration, authentication, session management |
| **Moderation** | Moderator and administrator tooling |
| **Storage** | SQLite database |
| **Backend** | Haskell + Warp web server |
| **Theming** | Custom Haskchan theme with dark/light mode |
| **Auth** | Encrypted cookies with expiration; revoked on account deletion |
| **Access control** | Registration can be disabled; board creation is globally configurable |
| **Deployment** | Self-hosted, with optional Tor onion-service support |

---

## Media Support

Haskchan detects uploaded files by inspecting their **contents**, not just the filename extension.

| Type | Formats |
|---|---|
| Image | JPEG, PNG, GIF, WebP |
| Video | WebM, MP4, QuickTime/MOV, M4V |
| Audio | OGG, FLAC, M4A, AAC, MP3 |
| Document | PDF |
| Text | TXT |

> **Note:** PDF files are detected via the `%PDF-` file signature and stored with the `.pdf` extension and `application/pdf` MIME type.

---

## Thumbnail Processing

Haskchan uses **FFmpeg** for media decoding and **ImageMagick** for thumbnail encoding.

```text
Uploaded image
     │
     ▼
FFmpeg decoder
     │
     ▼
Haskell image frame
     │
     ▼
Temporary PNG
     │
     ├──► ImageMagick ──► WebP thumbnail
     │
     └──► ImageMagick ──► JPEG thumbnail
```

This avoids the older `ffmpeg-light` encoder path, which can fail with newer FFmpeg versions.

**Verify ImageMagick is installed:**

```bash
which convert
```

Expected output:

```text
/usr/bin/convert
```

---

## Requirements

Haskchan currently works best on Linux (Ubuntu/Debian).

- GHC 8.10.7
- Stack
- SQLite
- ImageMagick
- FFmpeg 4.4.x libraries
- build-essential
- pkg-config
- zlib1g-dev

---

## Installation

### 1. Clone Haskchan

```bash
git clone https://github.com/konamicode01/haskchan.git
cd haskchan
```

### 2. Install system dependencies

On Ubuntu/Debian:

```bash
sudo apt update
sudo apt install -y \
  build-essential \
  pkg-config \
  sqlite3 \
  imagemagick \
  zlib1g-dev
```

Verify ImageMagick:

```bash
convert -version
```

### 3. Install FFmpeg 4.4.4

> **Note:** Haskchan uses the `ffmpeg-light` Haskell package, which relies on the **older FFmpeg 4.x API/ABI**. Newer FFmpeg versions may not be compatible.

Download and extract:

```bash
cd /root
wget https://ffmpeg.org/releases/ffmpeg-4.4.4.tar.xz
tar -xf ffmpeg-4.4.4.tar.xz
cd ffmpeg-4.4.4
```

Configure:

```bash
./configure \
  --prefix=/opt/ffmpeg4 \
  --libdir=/opt/ffmpeg4/lib \
  --enable-shared \
  --disable-static \
  --disable-asm \
  --disable-programs \
  --disable-doc
```

Build and install:

```bash
make -j"$(nproc)"
sudo make install
```

Register the libraries:

```bash
echo "/opt/ffmpeg4/lib" | sudo tee /etc/ld.so.conf.d/ffmpeg4.conf
sudo ldconfig
```

Set the library path:

```bash
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
```

To persist across sessions:

```bash
echo 'export LD_LIBRARY_PATH=/opt/ffmpeg4/lib' >> ~/.bashrc
source ~/.bashrc
```

### 4. Install Stack

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Verify:

```bash
stack --version
```

### 5. Build Haskchan

```bash
cd /root/haskchan
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack build
```

> **Note:** The first build may take a while — Stack downloads GHC and all Haskell dependencies. The repo contains a manually maintained `phi.cabal`; Stack may report that it's ignoring `package.yaml` in favor of the Cabal file — this is expected.

### 6. Run Haskchan

```bash
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack run
```

The development server starts on the address configured by the application.

---

## Authentication and Security

- Encrypted authentication cookies with expiration
- Authentication verifies the account still exists in the SQLite database
- Deleting a user account invalidates that user's existing session
- Registration can be fully disabled — when disabled, both the registration page and submission endpoint return:

  ```text
  403 Registration is closed
  ```

- Board creation is controlled **separately** via the global board-creation setting. Administrators can still create boards even when ordinary user board creation is disabled.

---

## CAPTCHA

- Included for account creation and posting
- **Click-to-refresh** — click the CAPTCHA image to get a fresh challenge without reloading the page
- Served with `no-cache` headers to prevent stale challenges from being reused by browsers or proxies

---

## Static File and MIME Type Support

Uploaded file types are detected from content and served with the correct MIME type:

| Extension | MIME Type |
|---|---|
| `.webp` | `image/webp` |
| `.jpg` / `.jpeg` | `image/jpeg` |
| `.png` | `image/png` |
| `.gif` | `image/gif` |
| `.svg` | `image/svg+xml` |
| `.ico` | `image/x-icon` |
| `.css` | `text/css` |
| `.js` | `text/javascript` |
| `.mp4` | `video/mp4` |
| `.webm` | `video/webm` |
| `.mp3` | `audio/mpeg` |
| `.ogg` | `audio/ogg` |
| `.flac` | `audio/flac` |
| `.pdf` | `application/pdf` |
| `.txt` | `text/plain` |

---

## Configuration

Project configuration lives in the Haskchan source tree. Before deploying publicly, configure:

- Database location
- Secret file
- Static file directories
- CAPTCHA directory
- Font path
- Server bind address
- Server ports
- Global site settings

> **Note:** Make sure the user running Haskchan has write permission to the directories used for uploaded files and generated thumbnails.

---

## Production Deployment

Haskchan can run directly via Warp, or behind a reverse proxy such as **nginx** or **Caddy**.

```text
Internet
   │
   ▼
nginx / Caddy
   │
   ▼
Haskchan
```

Or directly:

```text
Internet
   │
   ▼
Haskchan
```

For production, use a service manager such as **systemd** rather than running `stack run` in an interactive terminal.

**Example systemd unit:**

```ini
[Unit]
Description=Haskchan Imageboard
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/root/haskchan
Environment="LD_LIBRARY_PATH=/opt/ffmpeg4/lib"
ExecStart=/usr/local/bin/stack run
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
sudo systemctl daemon-reload
sudo systemctl enable haskchan
sudo systemctl start haskchan
```

Check status:

```bash
sudo systemctl status haskchan
```

View logs:

```bash
journalctl -u haskchan -f
```

---

## Tor Onion Service

Haskchan can optionally expose a local HTTP listener for a Tor onion service:

```text
Tor
 │
 ▼
127.0.0.1:7000
 │
 ▼
Haskchan
```

Example Tor configuration:

```text
HiddenServiceDir /var/lib/tor/haskchan/
HiddenServicePort 80 127.0.0.1:7000
```

This allows Haskchan to be reached via a `.onion` address without requiring the onion service to use the public TLS certificate for the clearnet domain.

---

## Updating Haskchan

Pull the latest source:

```bash
git pull origin main
```

Rebuild:

```bash
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack build
```

Restart the service:

```bash
sudo systemctl restart haskchan
```

Check status:

```bash
sudo systemctl status haskchan --no-pager
```

---

## Development

```bash
stack build   # Build
stack run     # Run
stack test    # Run tests
```

---

## Contributing

Haskchan is open source and contributions are welcome, including:

- Bug fixes
- Security improvements
- Performance improvements
- New media formats
- UI improvements
- Documentation
- Deployment improvements
- Haskell code improvements
- Tests

Before submitting changes, build the project and confirm existing functionality still works.

---

## Reporting Bugs

When reporting a bug, please include:

- Operating system
- GHC version
- Stack version
- FFmpeg version
- Relevant Haskchan logs
- Steps to reproduce the problem

> **Note:** Do not post private keys, authentication secrets, or other sensitive server configuration.

---

## License

See the [repository](https://github.com/konamicode01/haskchan) for the project's current license.

---

<div align="center">

**[Back to top](#haskchan)**

</div>
