Haskchan
Haskchan is a self-hosted imageboard written in Haskell.
It is designed to be lightweight, simple to deploy, and easy to modify.
Features
Imageboard-style threads and replies
Image uploads
Automatic WebP and JPEG thumbnails
CAPTCHA support
User accounts
Moderation/admin functionality
SQLite database
Haskell-based backend
Self-hosted deployment
Requirements
Haskchan currently works best on Linux systems such as Ubuntu and Debian.
You will need:
GHC 8.10.7
Stack
SQLite
ImageMagick
FFmpeg 4.4.x libraries
`build-essential`
`pkg-config`
Installation
1. Clone Haskchan
```bash
git clone https://github.com/konamicode01/haskchan.git
cd haskchan
```
2. Install system dependencies
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
You should have the `convert` command available.
3. Install FFmpeg 4.4.4
Haskchan uses the `ffmpeg-light` Haskell package. The version currently used by Haskchan relies on the older FFmpeg 4.x API and ABI.
FFmpeg 6.x is not compatible with this version of `ffmpeg-light`.
Download FFmpeg 4.4.4:
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
Build:
```bash
make -j"$(nproc)"
```
Install:
```bash
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
You can add the export to your shell configuration if you want it to persist:
```bash
echo 'export LD_LIBRARY_PATH=/opt/ffmpeg4/lib' >> ~/.bashrc
source ~/.bashrc
```
4. Install Stack
Install Stack if it is not already installed:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
Verify:
```bash
stack --version
```
5. Build Haskchan
From the Haskchan directory:
```bash
cd /root/haskchan
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack build
```
The first build may take some time because Stack downloads GHC and the required Haskell dependencies.
6. Run Haskchan
```bash
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack run
```
The development server will start on the address configured by the application.
Image Thumbnails
Haskchan uses FFmpeg for image decoding, but thumbnail encoding is handled by ImageMagick.
The thumbnail process is:
```text
Uploaded image
     |
     v
FFmpeg decoder
     |
     v
Haskell image frame
     |
     v
Temporary PNG
     |
     +----> ImageMagick ---> WebP thumbnail
     |
     +----> ImageMagick ---> JPEG thumbnail
```
This avoids the old `ffmpeg-light` encoder path that can fail with newer FFmpeg versions.
Verify ImageMagick is installed:
```bash
which convert
```
Expected:
```text
/usr/bin/convert
```
Configuration
Project configuration is stored in the Haskchan source tree.
Before deploying publicly, configure:
database location
secret values
static file directories
CAPTCHA settings
server bind address/port
Make sure the user running Haskchan has permission to write to directories used for uploaded files and generated thumbnails.
Production Deployment
For a public installation, run Haskchan behind a reverse proxy such as nginx or Caddy.
A typical setup is:
```text
Internet
   |
   v
nginx / Caddy
   |
   v
Haskchan
```
Run Haskchan:
```bash
cd /root/haskchan
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack run
```
For production, use a service manager such as systemd rather than keeping `stack run` in an interactive terminal.
Updating Haskchan
Pull the latest source:
```bash
git pull origin main
```
Rebuild:
```bash
export LD_LIBRARY_PATH=/opt/ffmpeg4/lib
stack build
```
Restart the Haskchan service/process after rebuilding.
Development
Build:
```bash
stack build
```
Run:
```bash
stack run
```
Run tests:
```bash
stack test
```
Project Structure
```text
haskchan/
├── app/
├── src/
├── static/
├── test/
├── package.yaml
├── phi.cabal
└── stack.yaml
```
The project currently retains the original `Phi.*` Haskell module namespace internally for compatibility, while the public application is branded Haskchan.
GitHub
Source code:
https://github.com/konamicode01/haskchan
License
See the repository for the project's current license.
