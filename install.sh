# SPOCK INSTALLER

#!/bin/sh

# If any command fails, stop the script immediately
set -e

INSTALL_DIR="/usr/local/bin"

# Only add colors if we're in a real terminal and not in a pipe
if [ -t 1 ]; then
    RED="\033[31m" GREEN="\033[32m" BOLD="\033[1m" RESET="\033[0m"
else
    RED="" GREEN="" BOLD="" RESET=""
fi

info()  { printf "${BOLD}[spock]${RESET} %s\n" "$*"; }
error() { printf "${RED}[spock] error:${RESET} %s\n" "$*" >&2; exit 1; }

if command -v curl >/dev/null 2>&1; then
    # Fail silently on HTTP errors & show errors even when silent & follow redirects & show progress bar
    DOWNLOAD_CMD="curl -fSL --progress-bar"
elif command -v wget >/dev/null 2>&1; then
    # Write output to stdout & show progress bar
    DOWNLOAD_CMD="wget -O- --show-progress"
else
    # Curl is installed by default on macOS so the error can only appear on a Linux system
    error "curl or wget is required"
fi

# "Darwin" on macOS, "Linux" on Linux.
OS=$(uname -s)

# "x86_64", "arm64", "aarch64".
ARCH=$(uname -m)

case "$OS" in
    Darwin)
        case "$ARCH" in
            x86_64)  ARTIFACT="spock-x86_64-apple-darwin" ;;
            arm64)   ARTIFACT="spock-aarch64-apple-darwin" ;;
            *)       error "Unsupported macOS architecture: $ARCH" ;;
        esac
        ;;
    Linux)
        case "$ARCH" in
            x86_64)
                # Quietly check if AVX2 is supported by the current CPU on Linux
                if grep -q avx2 /proc/cpuinfo 2>/dev/null; then
                    ARTIFACT="spock-x86_64-linux-v3"
                else
                    # Fallback for older CPUs
                    ARTIFACT="spock-x86_64-linux-v1"
                fi
                ;;
            aarch64) ARTIFACT="spock-aarch64-linux" ;;
            *)       error "Unsupported Linux architecture: $ARCH" ;;
        esac
        ;;
    *)
        # Windows will eventually be supported by a Powershell script or an installer
        error "Unsupported OS: $OS. On Windows, download the .zip from https://github.com/horacehoff/spock/releases/latest"
        ;;
esac

URL="https://github.com/horacehoff/spock/releases/latest/download/$ARTIFACT.tar.gz"

info "Detected platform: $OS/$ARCH → downloading $ARTIFACT"

# Create a temp directory
TMP=$(mktemp -d)

# Clean up the temp directory once the script exits, for ANY reason
trap 'rm -rf "$TMP"' EXIT

# Extract, decompress the gzip, and write it to $TMP/$ARTIFACT
$DOWNLOAD_CMD "$URL" | tar -xz -C "$TMP"

# If the downloaded archive contains a directory, return an error
if [ ! -f "$TMP/$ARTIFACT" ]; then
    error "Archive downloaded but binary not found inside. Please file a bug at https://github.com/horacehoff/spock/issues"
fi

if install -m755 "$TMP/$ARTIFACT" "$INSTALL_DIR/spock" 2>/dev/null; then
    :
elif command -v sudo >/dev/null 2>&1; then
    info "Asking for sudo to write to $INSTALL_DIR ..."
    sudo install -m755 "$TMP/$ARTIFACT" "$INSTALL_DIR/spock"
else
    error "Cannot write to $INSTALL_DIR and sudo is not available. Re-run as root or install sudo."
fi

printf "${GREEN}[spock]${RESET} Installed $("$INSTALL_DIR/spock" --version) in $INSTALL_DIR/spock\n"
printf "${GREEN}[spock]${RESET} Run 'spock' to get started.\n"