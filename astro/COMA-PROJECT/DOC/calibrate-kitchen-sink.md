# calibrate-image: kitchen-sink WCS and photometric calibration

The `calibrate-image` command can calibrate an image in a **kitchen-sink** mode. Instead of
using one reference catalog with fixed settings, it tries a sequence of catalogs and
settings in turn and stops at the first one that gives a good result. This is useful for
automated pipelines handling images from many fields and instruments, where the best
catalog is not known in advance.

WCS fitting and photometric calibration each have their own independent switch:

| Parameter | Default | Meaning |
|---|---|---|
| `WCS-KITCHEN-SINK` | `false` | Try the WCS sequence below until a fit succeeds |
| `PHOT-KITCHEN-SINK` | `false` | Try the photometric sequence below until a calibration succeeds |

With both off, `calibrate-image` behaves as before: one attempt with `WCS-CATALOG` and
`PHOT-CATALOG`.

## Quick start

```json
{
  "type": "request",
  "command": "calibrate-image",
  "id": "abc123",
  "parameters": {
    "FITS-FILE": "/data/image.fits",
    "WCS-KITCHEN-SINK": true,
    "PHOT-KITCHEN-SINK": true
  }
}
```

## WCS kitchen sink

Each attempt runs SExtractor and SCAMP against a reference catalog, using a given SCAMP
`POSITION_MAXERR` (how far, in arcmin, the true position may be from the image's initial
WCS). The attempts are tried in this order:

| # | Catalog | `POSITION_MAXERR` |
|---|---|---|
| 1 | refcat | 0.2′ |
| 2 | refcat | 1′ |
| 3 | refcat | 3′ |
| 4 | ps1-stack | 1′ |
| 5 | ps1-stack | 3′ |

The small search radius comes first because a tight search is less likely to produce a false match. The
search widens only if the tighter search fails.

**What counts as success.** An attempt succeeds only if it meets the same limits as a normal fit:
`WCS-NSTARS-MIN`, `WCS-RMS-MAX` and `WCS-FIT-NORDER`. These are **not** relaxed for later
attempts, because looser searches are more prone to false matches.

**Failed attempts don't change the image.** A failed SCAMP attempt doesn't write anything to the image,
so every attempt starts from the same initial WCS.

## Photometric kitchen sink

| # | Catalog | Standards used |
|---|---|---|
| 1 | refcat | stars only |
| 2 | ps1 | stars only |
| 3 | ps1-stack | stars only |
| 4 | ps1-stack | **stars and galaxies** |

The last attempt lets galaxies serve as brightness standards. This is often needed for faint
fields where there are not enough clearly stellar objects, and it is tried only after the
stars-only attempts fail. The response and FITS headers record whether galaxies were used
(see below).

SDSS is deliberately left out because of its limited sky coverage.

**What counts as success.** At least `PHOT-CALIB-NSTARS-MIN` standards and a calibration flagged `OK`
(a clean peak in the zero-point likelihood). A calibration returned with `OK = false`
moves on to the next attempt.

## Southern fields: PS1 is skipped south of dec −30°

Pan-STARRS (`ps1`, `ps1-stack`) does not cover the sky south of about −30°. In kitchen-sink
mode, any PS1 attempt is **skipped** when the image's approximate declination is south of
−30°. The declination is taken from the WCS reference point (CRVAL2), which is close enough
given the coarse survey boundary. Skipped attempts still appear in the attempts list with
the reason.

This applies only to the kitchen sinks. An explicitly requested `WCS-CATALOG` or
`PHOT-CATALOG` is always tried.

## Parameters that are ignored

When a kitchen sink is on, it chooses its own catalogs and search radius, so these are
**ignored** (and not checked), rather than causing an error:

| Ignored with `WCS-KITCHEN-SINK` | Ignored with `PHOT-KITCHEN-SINK` |
|---|---|
| `WCS-CATALOG`, `WCS-POSITION-MAXERR` | `PHOT-CATALOG` |

All other parameters (`WCS-NSTARS-MIN`, `WCS-RMS-MAX`, `WCS-FIT-NORDER`,
`WCS-ALLOW-SATURATED`, `PHOT-CALIB-NSTARS-MIN`, `REDO-*`, `CLEANUP`, …) apply as usual.

Without the kitchen sink, the WCS search radius can be set directly with
`WCS-POSITION-MAXERR` (arcmin, 0.01–60, default 2.0).

## Reusing previous results

As with normal calibration, a previous result stored in the FITS headers is reused unless
`REDO-WCS-FIT` / `REDO-PHOT-CALIB` is `true`:

- **WCS kitchen sink:** a previous COMA WCS fit is reused, whichever catalog produced it.
- **Photometric kitchen sink:** a previous calibration is reused only if it was `OK`;
  otherwise the sequence runs again.

When a previous result is reused, no attempts are made, so the response has no
`WCS-ATTEMPTS` / `PHOT-ATTEMPTS` list.

## Response

The usual `WCS-INFO` and `PHOT-CALIB-INFO` objects describe the result that was kept. Two
new fields record what was used:

| Field | In | Meaning |
|---|---|---|
| `WCS-POSITION-MAXERR` | `WCS-INFO` | The SCAMP search radius (arcmin) of the successful fit |
| `PHOT-STARS-ONLY` | `PHOT-CALIB-INFO` | `false` if galaxies were used as standards |

The response also lists every attempt, in order, in `WCS-ATTEMPTS` / `PHOT-ATTEMPTS`.
Without the kitchen sink this is a single entry for the one catalog tried. In that mode a
photometric calibration that is not `OK` is still kept and written, as before, so its entry
has `OK` `false`.

| Field | Present | Meaning |
|---|---|---|
| `CATALOG` | always | Catalog tried |
| `POSITION-MAXERR` | WCS | Search radius, arcmin |
| `STARS-ONLY` | photometry | Whether only stars were used |
| `OK` | always | `true` for the attempt that was kept |
| `SECONDS` | attempts that ran | Time taken |
| `NSTARS`, `RMSFIT` | successful WCS attempt | Fit quality |
| `NSTARS`, `ZPMAG` | photometry attempts that produced a result | Calibration result |
| `ERROR` | failed attempts | Why it failed |
| `SKIPPED` | skipped attempts | Why it was skipped (e.g. south of the PS1 limit) |

### Example: success

CFHT MegaCam image at dec +14.6° on a machine without a local refcat. Both kitchen sinks
fall back to PS1 (excerpt):

```json
"WCS-INFO": {
  "WCS-CATALOG": "ps1-stack",
  "WCS-POSITION-MAXERR": 1.0,
  "WCS-NSTARS": 67,
  "WCS-RMSFIT": 0.356,
  ...
},
"WCS-ATTEMPTS": [
  {"CATALOG": "refcat",    "POSITION-MAXERR": 0.2, "OK": false, "ERROR": "REFCAT:REFCAT_DIR not found ..."},
  {"CATALOG": "refcat",    "POSITION-MAXERR": 1.0, "OK": false, "ERROR": "REFCAT:REFCAT_DIR not found ..."},
  {"CATALOG": "refcat",    "POSITION-MAXERR": 3.0, "OK": false, "ERROR": "REFCAT:REFCAT_DIR not found ..."},
  {"CATALOG": "ps1-stack", "POSITION-MAXERR": 1.0, "OK": true,  "NSTARS": 67, "RMSFIT": 0.356, "SECONDS": 19.1}
],
"PHOT-CALIB-INFO": {
  "PHOT-CATALOG": "ps1",
  "PHOT-STARS-ONLY": true,
  "OK": true,
  "PHOT-NSTARS": 32,
  "ZPMAG": 31.724,
  ...
},
"PHOT-ATTEMPTS": [
  {"CATALOG": "refcat", "STARS-ONLY": true, "OK": false, "ERROR": "REFCAT:REFCAT_DIR not found ..."},
  {"CATALOG": "ps1",    "STARS-ONLY": true, "OK": true,  "NSTARS": 32, "ZPMAG": 31.724, "SECONDS": 6.1}
]
```

### Example: every attempt fails

If nothing succeeds, the request returns the usual error (`FAILED-TO-RUN-WCS-FIT` or
`PHOT-CALIB-FAILURE`). The attempts list is still included, so you can see why each try
failed:

```json
"ERROR": {
  "ERROR": "PHOT-CALIB-FAILURE",
  "DESCRIPTION": "All 4 photometric kitchen-sink attempts failed (see PHOT-ATTEMPTS); last error <calibration not OK (likelihood peak not clean)>"
},
"PHOT-ATTEMPTS": [
  {"CATALOG": "refcat",    "STARS-ONLY": true,  "OK": false, "ERROR": "REFCAT:REFCAT_DIR not found ..."},
  {"CATALOG": "ps1",       "STARS-ONLY": true,  "OK": false, "ERROR": "Found 32 catalog calibration stars but MIN-CALIB-STARS=40"},
  {"CATALOG": "ps1-stack", "STARS-ONLY": true,  "OK": false, "ERROR": "Found 27 catalog calibration stars but MIN-CALIB-STARS=40"},
  {"CATALOG": "ps1-stack", "STARS-ONLY": false, "OK": false, "ERROR": "calibration not OK (likelihood peak not clean)"}
]
```

A skipped attempt in a southern field looks like:

```json
{"CATALOG": "ps1-stack", "POSITION-MAXERR": 1.0, "OK": false,
 "SKIPPED": "skipped: image dec -45.2 is south of the PS1 limit -30.0"}
```

## FITS headers

The result is stored in the image's headers, as for a normal calibration. Two headers
are new:

| Header | Meaning |
|---|---|
| `COMA.WCS.MAXERR` | SCAMP `POSITION_MAXERR` (arcmin) of the successful WCS fit |
| `COMA.PHOT-CALIB.STARS-ONLY` | `F` if galaxies were also used as photometric standards |

`COMA.WCS.CATALOG` and `COMA.PHOT-CALIB.CATALOG` record which catalog won.

## Things to know

- **refcat must be available locally** (`REFCAT_DIR`). Where it isn't, the refcat attempts
  fail in about a second each and the kitchen sinks fall through to PS1.
- **Run time.** Each WCS attempt runs SExtractor and SCAMP. The first PS1 attempt also downloads
  the catalog (typically 10–20 s), and later attempts on the same field use the cached copy.
  When every attempt fails it can take a minute or more, because each SCAMP run has a 60 s timeout.
- **PS1 catalogs come from MAST**, through the COMA catalog code. They don't depend on SCAMP's own
  VizieR query.
- **Working files.** Each WCS attempt overwrites the previous attempt's working files in
  the image's `_DIR` directory. Use the attempts list in the response to see what happened.

## Changing the sequences (for maintainers)

The sequences and the PS1 limit are Lisp variables in
`astro/COMA-PROJECT/coma-json-server/`:

| Variable | File | Default |
|---|---|---|
| `*WCS-KITCHEN-SINK-SEQUENCE*` | `calib-wcs.lisp` | `(("refcat" 0.2) ("refcat" 1.0) ("refcat" 3.0) ("ps1-stack" 1.0) ("ps1-stack" 3.0))` |
| `*PHOT-KITCHEN-SINK-SEQUENCE*` | `calib-phot.lisp` | `(("refcat" t) ("ps1" t) ("ps1-stack" t) ("ps1-stack" nil))`. The second element is "stars only". |
| `*PS1-DEC-MIN*` | `calib-wcs.lisp` | `-30d0` |
| `*DEFAULT-WCS-POSITION-MAXERR*` | `calib-wcs.lisp` | `2.0` (arcmin) |
