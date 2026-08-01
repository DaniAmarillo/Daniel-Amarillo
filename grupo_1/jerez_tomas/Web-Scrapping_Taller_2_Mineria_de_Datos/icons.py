
_OUTLINE_ATTRS = (
    'xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" '
    'fill="none" stroke="currentColor" stroke-width="1.8" '
    'stroke-linecap="round" stroke-linejoin="round"'
)
_FILLED_ATTRS = (
    'xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" '
    'fill="currentColor"'
)

ICONS_OUTLINE = {
    "home": '<path d="M3 9.5L12 3l9 6.5V20a1 1 0 0 1-1 1h-5v-7h-6v7H4a1 1 0 0 1-1-1V9.5z"/>',
    "bar-chart": '<path d="M3 21h18M7 17V9M12 17V5M17 17v-7"/>',
    "activity": '<path d="M22 12h-4l-3 9L9 3l-3 9H2"/>',
    "trending-up": '<polyline points="22 7 13.5 15.5 8.5 10.5 2 17"/><polyline points="16 7 22 7 22 13"/>',
    "database": '<ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v6c0 1.66 4 3 9 3s9-1.34 9-3V5"/><path d="M3 11v6c0 1.66 4 3 9 3s9-1.34 9-3v-6"/>',
    "settings": '<circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 1 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 1 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 1 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.6 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 1 1 0-4h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 1 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.6 1.65 1.65 0 0 0 10 3.09V3a2 2 0 1 1 4 0v.09A1.65 1.65 0 0 0 15 4.6a1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 1 1 2.83 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 1 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z"/>',
    "refresh": '<polyline points="23 4 23 10 17 10"/><polyline points="1 20 1 14 7 14"/><path d="M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15"/>',
    "info": '<circle cx="12" cy="12" r="10"/><line x1="12" y1="16" x2="12" y2="12"/><line x1="12" y1="8" x2="12.01" y2="8"/>',

    "calendar": '<rect x="3" y="4" width="18" height="18" rx="2" ry="2"/><line x1="16" y1="2" x2="16" y2="6"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="3" y1="10" x2="21" y2="10"/>',
    "tag": '<path d="M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z"/><line x1="7" y1="7" x2="7.01" y2="7"/>',
    "user": '<path d="M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"/><circle cx="12" cy="7" r="4"/>',
    "link": '<path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"/><path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"/>',
    "search": '<circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/>',

    "download": '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/>',
    "play": '<polygon points="6 4 20 12 6 20 6 4"/>',
    "filter": '<polygon points="22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3"/>',
    "x": '<line x1="18" y1="6" x2="6" y2="18"/><line x1="6" y1="6" x2="18" y2="18"/>',
    "external-link": '<path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"/><polyline points="15 3 21 3 21 9"/><line x1="10" y1="14" x2="21" y2="3"/>',
}



ICONS_GEOMETRIC = {
    "diamond-filled": '<polygon points="12 2 22 12 12 22 2 12"/>',
    "diamond-outline": '<polygon points="12 2 22 12 12 22 2 12" fill="none" stroke="currentColor" stroke-width="2"/>',
    "triangle-up": '<polygon points="12 4 22 20 2 20"/>',
    "triangle-down": '<polygon points="2 4 22 4 12 20"/>',
    "triangle-right": '<polygon points="4 2 4 22 22 12"/>',
    "square-filled": '<rect x="4" y="4" width="16" height="16"/>',
    "square-outline": '<rect x="4" y="4" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2"/>',
    "hex": '<polygon points="12 2 21 7 21 17 12 22 3 17 3 7"/>',
    "circle-filled": '<circle cx="12" cy="12" r="9"/>',

    "corner-tl": '<polyline points="4 10 4 4 10 4" fill="none" stroke="currentColor" stroke-width="2.5"/>',
    "corner-tr": '<polyline points="14 4 20 4 20 10" fill="none" stroke="currentColor" stroke-width="2.5"/>',
    "corner-bl": '<polyline points="4 14 4 20 10 20" fill="none" stroke="currentColor" stroke-width="2.5"/>',
    "corner-br": '<polyline points="14 20 20 20 20 14" fill="none" stroke="currentColor" stroke-width="2.5"/>',

    "chart-timeline": '<path d="M3 18l5-5 4 4 9-9" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/><circle cx="3" cy="18" r="1.5"/><circle cx="8" cy="13" r="1.5"/><circle cx="12" cy="17" r="1.5"/><circle cx="21" cy="8" r="1.5"/>',
    "donut": '<circle cx="12" cy="12" r="9" fill="none" stroke="currentColor" stroke-width="3"/><circle cx="12" cy="12" r="4" fill="none" stroke="currentColor" stroke-width="2"/>',
    "people": '<circle cx="9" cy="8" r="3"/><circle cx="17" cy="9" r="2.5"/><path d="M3 21v-1a6 6 0 0 1 12 0v1M14 21v-1a5 5 0 0 1 8-4"/>',
    "bars-stack": '<rect x="3" y="14" width="4" height="7"/><rect x="10" y="10" width="4" height="11"/><rect x="17" y="6" width="4" height="15"/>',
    "scatter": '<circle cx="6" cy="18" r="1.5"/><circle cx="10" cy="12" r="1.5"/><circle cx="14" cy="15" r="1.5"/><circle cx="18" cy="7" r="1.5"/><circle cx="9" cy="6" r="1.5"/><circle cx="17" cy="17" r="1.5"/>',

    "online-dot": '<circle cx="12" cy="12" r="6"/>',
    "trophy": '<path d="M6 4h12v4a6 6 0 0 1-12 0V4z" fill="none" stroke="currentColor" stroke-width="2"/><path d="M9 18h6v3H9zM6 6H3v2a3 3 0 0 0 3 3M18 6h3v2a3 3 0 0 1-3 3"/>',
    "fire": '<path d="M12 2c1 4 3 5 3 8a3 3 0 0 1-6 0c0-1 .5-1.5 1-2-1 1-3 3-3 6a6 6 0 0 0 12 0c0-5-5-7-7-12z"/>',
    "lightning": '<polygon points="13 2 4 14 11 14 9 22 20 10 13 10 13 2"/>',
}


def svg(name: str, size: int = 18, css_class: str = "", stroke_width: float = None) -> str:

    if name in ICONS_OUTLINE:
        body = ICONS_OUTLINE[name]
        attrs = _OUTLINE_ATTRS
        if stroke_width is not None:
            attrs = attrs.replace('stroke-width="1.8"', f'stroke-width="{stroke_width}"')
        return (
            f'<svg width="{size}" height="{size}" {attrs} '
            f'class="cyber-icon {css_class}">{body}</svg>'
        )
    if name in ICONS_GEOMETRIC:
        body = ICONS_GEOMETRIC[name]
        if "<path" in body or "<polygon" in body or "<circle" in body or "<rect" in body or "<polyline" in body:
            if 'fill="none"' in body or 'stroke="currentColor"' in body:
                attrs = 'xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"'
            else:
                attrs = _FILLED_ATTRS
        else:
            attrs = _FILLED_ATTRS
        return (
            f'<svg width="{size}" height="{size}" {attrs} '
            f'class="cyber-icon cyber-icon-geo {css_class}">{body}</svg>'
        )
    return f'<!-- icon not found: {name} -->'


def icon_inline(name: str, label: str = "", size: int = 16, gap_em: float = 0.4) -> str:
    s = svg(name, size)
    if label:
        return (
            f'<span style="display:inline-flex; align-items:center; '
            f'gap:{gap_em}em; vertical-align:middle;">'
            f'{s}<span>{label}</span></span>'
        )
    return s


ICON_CSS = """
<style>
    .cyber-icon {
        vertical-align: middle;
        display: inline-block;
        color: #bd00ff;
        flex-shrink: 0;
    }
    .cyber-icon-geo {
        color: #00f0ff;
    }
    /* Variantes por contexto */
    .cyber-icon.accent { color: #00f0ff; }
    .cyber-icon.magenta { color: #ff006e; }
    .cyber-icon.success { color: #39ff14; }
    .cyber-icon.warning { color: #ffb800; }
    .cyber-icon.muted { color: #9d8bb8; }
</style>
"""


def inject_icon_css():
    import streamlit as st
    st.markdown(ICON_CSS, unsafe_allow_html=True)
