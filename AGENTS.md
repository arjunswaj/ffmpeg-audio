# ffmpeg-audio — Agent guide

## Prerequisites

- **FFmpeg 8** (`libavcodec`, `libavformat`, `libavutil`) installed via Homebrew, built with `--enable-libmp3lame`
- **GHC 9.10.3**, **cabal 3.16**, **hsc2hs** (via ghcup)
- **pkg-config** resolves ffmpeg headers/libs automatically

## Version control

This repo uses **JJ (Jujutsu) with git backend** — never use `git` directly. Use `jj` for all VCS operations.

- `jj log` / `jj status` to inspect state
- `jj describe` to edit commit descriptions
- `jj new` / `jj squash` / `jj split` for workflow
- `jj git push` to push to remote (uses git under the hood)
- The `.jj/` directory is the source of truth; `.git/` is a mirror managed by JJ

## Build & Test

```bash
cabal build             # build library
cabal test              # run all tests
cabal test --test-show-details=direct  # verbose test output
```

Tests are exitcode-stdio-1.0 (not tasty/hspec). `test/Main.hs` runs all specs sequentially; on failure an exception is thrown immediately.

## Project structure

```
src/FFmpeg/Audio/
  Main module, PCMBuffer, Encoder  — public API (exposed)
  Internal/FFI, Types, Error, Codec,
    CodecContext, Frame, Packet,
    Format, Encoder                 — internals (also exported)
```

**Internal pattern**: each FFmpeg object (AVFrame, AVPacket, AVCodecContext, AVFormatContext) has a `Handle` newtype wrapping a `ForeignPtr`, with `allocate`/`free`/`with` functions and a `bracket`-based `with*` helper.

## Key architecture

- **`c_encode_*`** FFI imports live in `Internal/FFI.hsc`
- **Opaque types** (`data AVFormatContext`, etc.) and `#{enum}`/`#{offset}` macros in `Internal/Types.hsc`
- **Error handling**: `FFmpegError` (Exception) via `errorNonNegative` helper; `avErrorToString` for human-readable av_strerror messages
- **Encoder pipeline**: `findMp3Encoder` → `withCodecContext` → `withOutputFile` → send frame data → `flushEncoder`
- Frame size is hardcoded to 1152 samples (MP3 frame size)
- Uses planar S16 sample format (`AV_SAMPLE_FMT_S16P`) — deinterleaves PCM on the Haskell side
- Bitrate hardcoded to 320kbps

## Supported API

- `encodeMp3 :: FilePath -> PCMBuffer -> IO ()`
- `encodeMp3LBS :: PCMBuffer -> IO BS.ByteString`
- Individual internal handles can be used directly: `findMp3Encoder`, `withCodecContext`, `withFrame`, `withPacket`, `withOutputFile`

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