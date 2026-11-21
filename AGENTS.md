# ffmpeg-audio — Agent guide

## Prerequisites

Everything runs inside the Docker devcontainer (`.devcontainer/Dockerfile` on
`debian:trixie-slim` with FFmpeg 7.1+, GHC 9.10.3, cabal 3.16.1.0, ormolu, hlint,
git 2.54.0, and jj v0.46.0). Never install or run GHC, cabal, or Python on the
macOS host; run all commands through `.devcontainer/exec.sh`.

- **FFmpeg 7.1+** (`libavcodec`, `libavformat`, `libavutil`, `libswresample`, `ffmpeg`/`ffprobe`)
- **GHC 9.10.3**, **cabal 3.16.1.0**, **hsc2hs** (via ghcup inside the container)
- **pkg-config** resolves ffmpeg headers/libs automatically

## Version control

This repo uses **JJ (Jujutsu) with git backend** — never use `git` directly. Use `jj` inside the container via `.devcontainer/exec.sh` for all VCS operations.

- `bash .devcontainer/exec.sh jj log` / `bash .devcontainer/exec.sh jj status` to inspect state
- `bash .devcontainer/exec.sh jj describe` to edit commit descriptions
- `bash .devcontainer/exec.sh jj new` / `jj squash` / `jj split` for workflow
- `bash .devcontainer/exec.sh jj git push` to push to remote (uses git under the hood)
- The `.jj/` directory is the source of truth; `.git/` is a mirror managed by JJ

## Build & Test

All build, test, lint, format, and VCS commands run inside the devcontainer via `.devcontainer/exec.sh`:

```bash
bash .devcontainer/exec.sh cabal build                             # build library
bash .devcontainer/exec.sh cabal test                              # run all tests
bash .devcontainer/exec.sh cabal test --test-show-details=direct   # verbose test output
bash .devcontainer/exec.sh --workspace <name> cabal test           # run in a jj workspace
```

The first invocation builds `ffmpeg-audio-dev:local` and warms cabal dependencies
into the named Docker volumes `ffmpeg-audio-dist-newstyle` and `ffmpeg-audio-cabal-store`.

Tests are exitcode-stdio-1.0 (not tasty/hspec). `test/Main.hs` runs all specs sequentially; on failure an exception is thrown immediately.

## Project structure

```
src/FFmpeg/Audio/
  Main module, PCMBuffer, Encoder, IAMF  — public API (exposed)
  Internal/FFI, Types, Error, Codec,
    CodecContext, Frame, Packet,
    Format, Encoder, Iamf, StreamGroup   — internals (also exported)
```

**Internal pattern**: each FFmpeg object (`AVFrame`, `AVPacket`, `AVCodecContext`, `AVFormatContext`, `AVIAMFAudioElement`, `AVIAMFMixPresentation`, `AVIAMFParamDefinition`) has a `Handle` newtype wrapping a `ForeignPtr`, with `allocate`/`free`/`with` functions and a `bracket`-based `with*` helper.

## Key architecture

- **`c_*`** FFI imports live in `Internal/FFI.hsc`
- **Opaque types** (`data AVFormatContext`, `data AVIAMFAudioElement`, etc.) and `#{enum}`/`#{offset}` macros in `Internal/Types.hsc`
- **Error handling**: `FFmpegError` (Exception) via `errorNonNegative` helper; `avErrorToString` for human-readable `av_strerror` messages
- **Legacy MP3 pipeline**: `findMp3Encoder` → `withCodecContext` → `withOutputFile` → send S16P frames (1152 samples, 320 kbps) → `flushEncoder`
- **General multi-channel / high-res pipeline**: `encodeAudioToFile` dynamically reads `ctx->frame_size` (with variable-frame-size and fixed-frame-size tail padding), uses `AVFrame.extended_data` for >8 channels, sets explicit `AVChannelLayout` masks/Ambisonics strings, supports S16/S32 (24-bit MSB-aligned)/Float inputs, and rescales packet timestamps in Haskell `Integer` arithmetic
- **Native IAMF / Eclipsa Audio pipeline**: `FFmpeg.Audio.IAMF` splits channel-based (up to 7.1.4) and Ambisonics elements into mono/coupled-stereo substreams, creates `AVStreamGroup` audio elements & mix presentations via `Internal/Iamf.hsc` and `Internal/StreamGroup.hsc`, and muxes `.iamf` or `.mp4`

## Supported API

- `encodeMp3 :: FilePath -> PCMBuffer -> IO ()`
- `encodeMp3LBS :: PCMBuffer -> IO BS.ByteString`
- `encodeAudio :: FilePath -> CodecConfig -> AudioBuffer -> IO ()`
- `encodeAudioBS :: CodecConfig -> AudioBuffer -> IO BS.ByteString`
- `encodeFloatMp3 :: FilePath -> PCMBufferFloat -> IO ()`
- `encodeFlac24 :: FilePath -> AudioBuffer -> IO ()`
- `encodeOpus :: FilePath -> CodecConfig -> AudioBuffer -> IO ()`
- `encodeIamf :: FilePath -> IamfConfig -> [IamfElement] -> IO ()`
- `encodeIamfBS :: IamfConfig -> [IamfElement] -> IO BS.ByteString`
- Individual internal handles can be used directly: `findMp3Encoder`, `findEncoderByName`, `withCodecContext`, `withCodecContextWith`, `withFrame`, `withFrameWithLayout`, `withPacket`, `withOutputFile`, `withOutputFileWith`, `withIamfAudioElement`, `withIamfMixPresentation`, `withIamfParamDefinition`, `createStreamGroup`

## Adding more APIs

1. Add `foreign import` for the FFmpeg C function in `Internal/FFI.hsc`
2. Add any new opaque types or enum values in `Internal/Types.hsc` (use `#{enum}` or `#{const}`)
3. Create a Handle module if needed (bracket pattern: allocate/free/with)
4. Expose via `Internal/` module and optionally re-export from the public `FFmpeg.Audio` or `Encoder`

## Conventions

- **GHC2024** language default, `-Wall`
- No comments in code (style guide)
- Test modules export a `tests :: IO ()` function
- Temporary test files go to `/tmp/`
- Uses `Foreign.Concurrent.newForeignPtr` for FFI finalizers