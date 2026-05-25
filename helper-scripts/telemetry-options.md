# Telemetry Options for ausplotsR

## What I Want

- When any of **thousands of users** runs `ausplotsR` and hits an error,
  that error (message, function name, R version, OS) should appear **in my
  Sentry account** automatically.
- Users do not need to configure anything — it just works out of the box.
- Users can see that something is being logged (no hidden tracking), but they
  don't need to know the details.
- Package must be submittable to **CRAN**.

---

## What Is Possible with `otel`

| Capability | Reality |
|---|---|
| Structured logging API (`log_info`, `log_warn`, `log_error`) | ✅ Works, zero deps |
| No-op when no backend configured | ✅ Safe to ship |
| Send logs to **your** Sentry | ❌ **Not possible** — `otel` is API only; it does nothing without the user installing and configuring `otelsdk` |
| `otelsdk` installable without system packages | ❌ Requires `libprotobuf-dev` (C++ compile) |
| Users self-configure a backend | ❌ Will never happen in practice for a data-science package |

**Bottom line:** `otel` alone gives you zero telemetry in practice. Errors stay
on the user's machine and never reach your Sentry.

---

## What Is Preferred: Direct Sentry HTTP (via `httr2`)

Call Sentry's **Store API** directly from the package using `httr2`, which is
already on CRAN and has no system dependencies.

### How It Works

```
User hits an error in ausplotsR
        ↓
.sentry_capture_exception(e)  — always called internally
        ↓
  opt-out check: getOption("ausplotsR.telemetry", TRUE)
        ↓ (if TRUE)
  httr2::request("https://sentry.io/api/<project>/store/")
  httr2::req_body_json(list(exception, platform, contexts))
  httr2::req_perform()        — fire-and-forget, timeout = 2s
        ↓
  Event appears in YOUR Sentry dashboard
```

### CRAN Requirements Met

| CRAN Rule | How It Is Satisfied |
|---|---|
| No `Remotes:` field | `httr2` is on CRAN, no GitHub deps |
| No `install.packages()` in package code | `httr2` is a normal `Imports:` dep |
| Must not phone home without disclosure | `.onAttach` shows one opt-out message |
| Opt-out must be possible | `options(ausplotsR.telemetry = FALSE)` disables all POSTs |
| Network calls must be non-blocking | `httr2::req_timeout(2)` + `tryCatch` — never delays the user |

### Attach Message (shown once on `library(ausplotsR)`)

```
ausplotsR collects anonymous error data to help improve the package.
To opt out: options(ausplotsR.telemetry = FALSE)
```

### What Each Sentry Event Contains

- Error message and class
- Function name where the error occurred
- R version, OS, ausplotsR version
- Timestamp
- **No** user data, file paths, or API credentials

### Implementation Plan

1. **`R/sentry_helper.R`** — replace `otel::log_*` calls with a thin
   `httr2`-based POST to your DSN; keep `.sentry_capture_exception()` and
   `.sentry_capture_message()` signatures identical so no other files change.
2. **`R/set_auth.R` `.onAttach`** — add the single opt-out disclosure message.
3. **`DESCRIPTION`** — remove `otel` from `Imports:`, add `httr2`.
4. **`NAMESPACE`** — swap `importFrom("otel", ...)` for `importFrom("httr2", ...)`.
5. **`tests/testthat/test-sentry_integration.R`** — stub `httr2::req_perform`
   instead of `otel::log_*`; assert payload fields.
6. **`tests/testthat/test-set_auth.R`** — update `.onAttach` test to check
   disclosure message is printed.

### DSN Storage

Store the DSN as a package-internal constant in `sentry_helper.R`:

```r
.SENTRY_DSN <- "https://<key>@o<org>.ingest.sentry.io/<project>"
```

It is **not a secret** — Sentry DSNs are designed to be public (write-only,
cannot read events back). Any CRAN reviewer can see it; that is normal and
accepted practice (e.g. RStudio/Posit does the same for crash reporting).

---

## Alternative Approaches Considered

### Why a Network Call Is Unavoidable

Any approach that gets data from the user's machine to your server requires a
network call. The protocol changes, but not the fact.

### Alternatives to Sentry

| Approach | Protocol | CRAN OK | Dependencies | Notes |
|---|---|---|---|---|
| `httr2` POST to Sentry | HTTPS | ✅ | `httr2` | **Recommended** |
| `curl` package | HTTPS | ✅ | `curl` | Same result as httr2 |
| Base R `url()` ping | HTTPS | ✅ | None | Querystring only, no JSON body |
| ntfy.sh push notification | HTTPS | ✅ | None | Phone alerts, no error grouping |
| Discord webhook | HTTPS | ✅ | `httr2` | Raw event dump, no grouping |
| Axiom log ingestion | HTTPS | ✅ | `httr2` | Good search, no error grouping |
| Supabase REST insert | HTTPS | ✅ | `httr2` | You build the UI yourself |
| `otel` + `otelsdk` | HTTPS/gRPC | ✅ | Needs `libprotobuf-dev` | Users must configure — never happens |
| `sentryR` | HTTPS | ❌ | `Remotes:` | Not on CRAN |
| Local file log | — | ✅ | None | Errors stay on user's machine |

#### Base R `url()` — zero dependencies
```r
.ping_error <- function(msg) {
  tryCatch({
    old <- options(timeout = 2)
    on.exit(options(old))
    close(url(paste0("https://your-server.com/e?m=", URLencode(msg, TRUE))))
  }, error = function(e) invisible(NULL))
}
```
Your server logs the querystring. No packages added, but no structured error
data — only a URL-encoded string.

#### ntfy.sh — phone push notification, base R only
```r
url(paste0("https://ntfy.sh/ausplotsR-errors-xyz/publish?message=",
           URLencode(msg, TRUE)))
```
Free, instant phone alert. Good for low-volume critical errors. Falls apart
at thousands of events — no grouping, no deduplication, inbox floods.

#### Discord webhook
Write-only URL posted to a private channel. Free forever. Searchable in
Discord. Still just raw text dumps — 1000 users hitting the same bug = 1000
separate messages.

#### Axiom
Log ingestion with an excellent query UI and a generous free tier (500 GB/month).
Good for log search and dashboards, not designed for error monitoring.
You would need to build deduplication and grouping yourself.

---

## Final Recommendation: Sentry via `httr2`

### Why Sentry Is the Best Tool for This Job

| Feature | Sentry | All Alternatives |
|---|---|---|
| Error grouping (1000 users, 1 bug = 1 issue) | ✅ | ❌ |
| Stack traces | ✅ | ❌ |
| First seen / last seen / frequency trends | ✅ | ❌ |
| Resolved / ignored / assigned workflow | ✅ | ❌ |
| R version and OS breakdown per issue | ✅ | ❌ |
| Free tier | ✅ 5k errors/month | ✅ |
| Designed for this exact problem | ✅ | ❌ |

The alternatives are useful tools for different problems:
- **ntfy.sh / Discord** — good for pinging you when something happens; not for
  managing errors at volume
- **Axiom** — good for log analytics; you would have to rebuild error grouping
  yourself
- **Supabase** — good for structured data; you would have to build the entire
  monitoring UI yourself
- **Base R `url()`** — good for a simple ping with zero deps; too limited for
  structured error payloads

**Sentry is the only option that gives you error grouping, deduplication, stack
traces, and a managed workflow — out of the box, for free, with a 25-line
implementation.**

---

## Decision Summary

| Option | Errors reach your Sentry | CRAN OK | User config needed | Recommended |
|---|---|---|---|---|
| `sentryR` (original) | ✅ | ❌ | No | ❌ |
| `otel` (current) | ❌ | ✅ | Yes (never happens) | ❌ |
| ntfy.sh / Discord | Partial | ✅ | No | ❌ |
| Axiom / Supabase | Partial | ✅ | No | ❌ |
| **`httr2` → Sentry** | ✅ | ✅ | No | ✅ |

---

## Decision Summary

| Option | Errors reach your Sentry | CRAN OK | User config needed |
|---|---|---|---|
| `sentryR` (original) | ✅ | ❌ (`Remotes:`) | No |
| `otel` (current) | ❌ | ✅ | Yes (never happens) |
| **`httr2` direct** (recommended) | ✅ | ✅ | **No** |
