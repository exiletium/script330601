"""Regenerate the bundled reference data in ``crates/engine/data``.

The engine itself has no runtime dependency on these packages; this script is
only needed when refreshing the vendored JSON files.

    pip install geonamescache mendeleev
    python tools/build_data.py

Sources
-------
* Countries and cities: GeoNames (https://www.geonames.org, CC BY 4.0) via the
  ``geonamescache`` package (MIT).
* Chemical elements: the ``mendeleev`` package (MIT), which aggregates IUPAC
  and other published reference values.
"""
from __future__ import annotations

import json
import unicodedata
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "crates" / "engine" / "data"

CONTINENTS = {
    "AF": "Africa", "AS": "Asia", "EU": "Europe", "NA": "North America",
    "SA": "South America", "OC": "Oceania", "AN": "Antarctica",
}


def ascii_fold(s: str) -> str:
    return "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))


def build_geo() -> None:
    import geonamescache

    g = geonamescache.GeonamesCache()
    countries = {}
    capitals = set()
    for iso, c in g.get_countries().items():
        countries[iso] = {
            "name": c["name"],
            "iso2": iso,
            "iso3": c["iso3"],
            "capital": c["capital"],
            "continent": CONTINENTS.get(c["continentcode"], c["continentcode"]),
            "area_km2": c["areakm2"],
            "population": c["population"],
            "tld": c["tld"],
            "currency_code": c["currencycode"],
            "currency_name": c["currencyname"],
            "calling_code": c["phone"],
            "languages": c["languages"],
            "neighbours": [n for n in c["neighbours"].split(",") if n],
        }
        if c["capital"]:
            capitals.add((iso, ascii_fold(c["capital"]).lower()))
    (OUT / "countries.json").write_text(json.dumps(countries, ensure_ascii=False, separators=(",", ":")))

    cities = []
    for c in g.get_cities().values():
        key = (c["countrycode"], ascii_fold(c["name"]).lower())
        is_capital = key in capitals
        if c["population"] < 100_000 and not is_capital:
            continue
        cities.append([
            c["name"], c["countrycode"], round(c["latitude"], 4), round(c["longitude"], 4),
            c["population"], c["timezone"], 1 if is_capital else 0, c.get("admin1code", ""),
        ])
    cities.sort(key=lambda r: -r[4])
    (OUT / "cities.json").write_text(json.dumps(
        {"fields": ["name", "country", "lat", "lon", "population", "timezone", "capital", "admin1"],
         "rows": cities}, ensure_ascii=False, separators=(",", ":")))
    print(f"countries: {len(countries)}  cities: {len(cities)}")


def build_elements() -> None:
    from mendeleev import element

    rows = []
    for z in range(1, 119):
        e = element(z)

        def num(v, nd=6):
            return None if v is None else round(float(v), nd)

        ox = []
        try:
            ox = sorted({int(s) for s in (e.oxistates or [])})
        except Exception:
            pass
        rows.append({
            "z": z,
            "symbol": e.symbol,
            "name": e.name,
            "mass": num(e.atomic_weight),
            "group": e.group_id,
            "period": e.period,
            "block": e.block,
            "series": e.series,
            "config": str(e.ec) if e.ec is not None else None,
            "electronegativity": num(e.en_pauling, 3),
            "melting_k": num(e.melting_point, 3),
            "boiling_k": num(e.boiling_point, 3),
            "density_g_cm3": num(e.density, 6),
            "atomic_radius_pm": num(e.atomic_radius, 2),
            "covalent_radius_pm": num(getattr(e, "covalent_radius_pyykko", None), 2),
            "vdw_radius_pm": num(e.vdw_radius, 2),
            "first_ionization_ev": num((e.ionenergies or {}).get(1), 4),
            "electron_affinity_ev": num(e.electron_affinity, 4),
            "discovery_year": e.discovery_year,
            "discoverers": e.discoverers,
            "oxidation_states": ox,
            "abundance_crust_mg_kg": num(e.abundance_crust, 6),
            "is_radioactive": bool(e.is_radioactive),
            "color": e.cpk_color,
            "name_origin": e.name_origin,
            "uses": e.uses,
            "description": e.description,
        })
    (OUT / "elements.json").write_text(json.dumps(rows, ensure_ascii=False, separators=(",", ":")))
    print(f"elements: {len(rows)}")


if __name__ == "__main__":
    OUT.mkdir(parents=True, exist_ok=True)
    build_geo()
    build_elements()
