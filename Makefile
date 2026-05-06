# Makefile for IPBES/IPCC Glossary
# Run all targets from the repo root.

RSCRIPT := Rscript

.PHONY: help \
        run-glossary run-compare \
        data update-ipcc caches sync-caches \
        manifest-glossary manifest-compare manifests \
        renv-glossary renv-compare

# ── Default ───────────────────────────────────────────────────────────────────
help:
	@echo ""
	@echo "IPBES/IPCC Glossary — available targets"
	@echo ""
	@echo "  run-glossary        Start the Glossary Explorer locally  (port 7654)"
	@echo "  run-compare         Start the Comparison app locally     (port 7655)"
	@echo ""
	@echo "  data                Full rebuild: clean IPBES CSV + scrape IPCC +"
	@echo "                      rebuild all caches + sync to app dirs"
	@echo "                      Slow (~5-10 min), requires network (apps.ipcc.ch)"
	@echo ""
	@echo "  update-ipcc         Scrape fresh IPCC data only, then rebuild caches"
	@echo "                      Use when IPCC glossary has been updated upstream"
	@echo ""
	@echo "  caches              Rebuild caches from existing CSVs — fast, no network"
	@echo "                      Use after code changes that affect cache structure"
	@echo ""
	@echo "  sync-caches         Copy inst/extdata/ caches to both app subdirs"
	@echo ""
	@echo "  manifest-glossary   Regenerate glossary/manifest.json"
	@echo "  manifest-compare    Regenerate compare/manifest.json"
	@echo "  manifests           Regenerate both manifest.json files"
	@echo "                      Run after any renv.lock / dependency changes"
	@echo ""
	@echo "  renv-glossary       Snapshot glossary/renv.lock from current library"
	@echo "  renv-compare        Snapshot compare/renv.lock from current library"
	@echo ""

# ── Run apps locally ──────────────────────────────────────────────────────────
run-glossary:
	cd glossary && $(RSCRIPT) -e "shiny::runApp(port=7654, launch.browser=FALSE)"

run-compare:
	cd compare && $(RSCRIPT) -e "shiny::runApp(port=7655, launch.browser=FALSE)"

# ── Data / cache targets ──────────────────────────────────────────────────────

# Full rebuild: re-processes IPBES CSV, scrapes IPCC, rebuilds everything.
data:
	$(RSCRIPT) data-raw/prepare_data.R

# Scrape fresh IPCC data from apps.ipcc.ch, then rebuild caches.
update-ipcc:
	$(RSCRIPT) inst/scripts/scrape_ipcc_and_update_caches.R

# Rebuild merged + hierarchy caches from existing CSVs (no network, fast).
caches:
	$(RSCRIPT) inst/scripts/update_bundled_caches.R

# Copy canonical inst/extdata/ caches into both app dirs.
sync-caches:
	$(RSCRIPT) inst/scripts/update_bundled_caches.R --force

# ── Manifest targets (per-app + combined) ─────────────────────────────────────
manifest-glossary:
	$(RSCRIPT) scripts/write_manifests.R glossary

manifest-compare:
	$(RSCRIPT) scripts/write_manifests.R compare

manifests: manifest-glossary manifest-compare

# ── renv targets (per-app) ────────────────────────────────────────────────────
renv-glossary:
	cd glossary && $(RSCRIPT) -e "renv::snapshot()"

renv-compare:
	cd compare && $(RSCRIPT) -e "renv::snapshot()"
