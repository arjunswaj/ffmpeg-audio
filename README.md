# ffmpeg-audio

Minimal, production-quality Haskell bindings to FFmpeg 8 for MP3 audio encoding.

Supports encoding signed 16-bit interleaved PCM audio into MP3 format using FFmpeg's built-in `libmp3lame` encoder — with no external binary dependency at runtime beyond the FFmpeg shared libraries.

## Prerequisites

- [FFmpeg 8.x](https://ffmpeg.org/) installed via Homebrew, built `--enable-libmp3lame`
- [GHC 9.10.x](https://www.haskell.org/ghc/), [cabal 3.16+](https://www.haskell.org/cabal/), [hsc2hs](https://hackage.haskell.org/package/hsc2hs) via ghcup
- `pkg-config` must be able to resolve `libavcodec`, `libavformat`, `libavutil`

Verify with:

```bashbrew install ffmpeg --enable-libmp3lame # if building from source
pkg-config --exists libavcodec && echo "OK"
```

## Getting started

Add to your `.cabal` file:

```cabal library ffmpeg-audio ...
    build-depends: ffmpeg-audio >=0.1.0.0, vector, bytestring
    pkgconfig-depends: libavcodec, libavformat,libavutil
```

Basic usage:

```haskell
import FFmpeg.Audio (PCMBuffer(..), encodeMp3, encodeMp3LBS)
import qualified Data.Vector as V, Data.ByteString as BS ...

-- PCMBuffer: sampleRate, channels, interleaved Int16 samples
let buf = PCMBuffer 44100 2 (V.replicate (1152 * 2) 0)
encodeMp3 "output.mp3" buf     -- writes mp3 file directly
mp3Data <- encodeMp3LBS buf       -- returns encoded mp3 as ByteString
```

More examples can be found in the test suite at `test/EncoderSpec.hs`.

## API reference

| Function | Signature | Description |
|---|---|---|
| `encodeMp3` | `FilePath -> PCMBuffer -> IO ()` | Encodes PCM data to an MP3 file on disk |
| `encodeMp3LBS` | `PCMBuffer -> IO ByteString` | Encodes PCM data to an MP3 `ByteString` in memory (via temp file internally) |

The `PCMBuffer` data type holds signed 16-bit interleaved little-endian PCM data:

```haskell
data PCMBuffer = PCMBuffer {
        pcmSampleRate ::           Int           -- e.g. 44100
      , pcmChannels   ::           Int           -- 1 = mono, 2 = stereo etc.
      , pcmSamples    :: Vector Int16               -- interleaved samples
    }
```

The internal handles (`findMp3Encoder`, `withCodecContext`, `withOutputFile`, `withFrame`, `withPacket`) are also exported and can be used directly for any FFmpeg-based custom pipeline — not limited to MP3 encoding.

## Contributing

The library is structured for easy extension to support additional encoders and media formats. To add support for a new codec/format:

1. **Add new foreign imports** — Add any needed FFmpeg C function to `Internal/FFI.hsc`
2. **Declare opaque types** — If you need new FFmpeg data types, declare them in `Internal/Types.hsc` and add enum constants via `#{enum}`
3. **Create a Handle module (optional)** — Use the established bracket pattern if creating a new FFmpeg object (allocate / free / finalize pattern via `newForeignPtr` + `bracket`)
4. **Expose the API** — Add new functions via an `Internal/` module, and optionally re-export from `FFmpeg.Audio`

### Style conventions

- `-Wall` with **GHC2024**
 - Project uses `hsc2hs` for compile-time C header extraction (`*.hsc` files are processed by `hsc2hs` via cabal's `build-tool-depends`)
 - Tests are `exitcode-stdio-1.0`, not tasty/hspec. Each `tests :: IO()` runs sequentially and throws on failure.
 - Temporary files go to `/tmp/`
 - Follows the FFI safety pattern already in use (`Foreign.Concurrent.newForeignPtr` for finalizers)

## Version control

This repo uses **JJ (Jujutsu) with a git backend** — never use `git` directly. Use `jj` for all VCS operations (log, status, describe, new, squash, split, push).

## License

`LGPL-2.1-or-later` — see LICENSE file for details.