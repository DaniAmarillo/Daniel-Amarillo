
import pandas as pd
import streamlit as st
import plotly.express as px
import plotly.graph_objects as go

from icons import svg, ICON_CSS


COLORS = {
    "bg":         "#0a0612",
    "surface":    "#1a0d2e",
    "surface_2":  "#241640",
    "border":     "#3d1f6b",
    "primary":    "#bd00ff",  
    "accent":     "#00f0ff",   
    "secondary":  "#ff006e",   
    "glow":       "#7b2cbf",   
    "neon_green": "#39ff14",
    "neon_yellow":"#ffb800",
    "text":       "#e8d8ff",   
    "text_muted": "#9d8bb8",   
}

TOPIC_COLORS = {
    "Machine Learning":  "#bd00ff",
    "IA Generativa":     "#00f0ff",
    "Estadística":       "#39ff14",
    "Otros":             "#ff006e",
}

PLOTLY_THEME = {
    "layout": {
        "paper_bgcolor": COLORS["surface"],
        "plot_bgcolor":  COLORS["bg"],
        "font": {"color": COLORS["text"], "family": "JetBrains Mono, Consolas, monospace"},
        "title": {"font": {"size": 16, "color": COLORS["primary"]}},
        "xaxis": {
            "gridcolor": COLORS["border"], "zerolinecolor": COLORS["glow"],
            "tickfont": {"color": COLORS["text_muted"]},
            "title": {"font": {"color": COLORS["accent"]}},
            "linecolor": COLORS["glow"],
        },
        "yaxis": {
            "gridcolor": COLORS["border"], "zerolinecolor": COLORS["glow"],
            "tickfont": {"color": COLORS["text_muted"]},
            "title": {"font": {"color": COLORS["accent"]}},
            "linecolor": COLORS["glow"],
        },
        "legend": {"font": {"color": COLORS["text"]},
                   "bgcolor": COLORS["surface_2"],
                   "bordercolor": COLORS["border"], "borderwidth": 1},
        "margin": {"l": 50, "r": 30, "t": 50, "b": 40},
    }
}


CUSTOM_CSS = """
<style>
    @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600;700&family=Orbitron:wght@600;700;900&display=swap');

    /* Fondo principal */
    .stApp {
        background:
            radial-gradient(ellipse at top left, rgba(189, 0, 255, 0.15) 0%, transparent 50%),
            radial-gradient(ellipse at top right, rgba(0, 240, 255, 0.10) 0%, transparent 50%),
            radial-gradient(ellipse at bottom, rgba(255, 0, 110, 0.08) 0%, transparent 50%),
            #0a0612;
        background-attachment: fixed;
    }

    /* Sidebar */
    section[data-testid="stSidebar"] {
        background: linear-gradient(180deg, #1a0d2e 0%, #100820 100%);
        border-right: 2px solid #bd00ff;
        box-shadow: 4px 0 30px rgba(189, 0, 255, 0.3);
    }
    section[data-testid="stSidebar"] * {
        color: #e8d8ff;
    }

    .stApp, .stApp p, .stApp label, .stApp span, .stApp div {
        font-family: 'JetBrains Mono', monospace !important;
        color: #e8d8ff;
    }

    /* === Plan B: ocultar solo los iconos problemáticos === */
    /* Streamlit usa la fuente Material Symbols Rounded que no carga
       consistentemente. Solo ocultamos los iconos que causaban texto
       crudo ("keyboard_double_arrow_left", "expand_more"). Los iconos
       definidos vía :material/name: en st.Page SÍ los queremos visibles. */
    
    /* Botón de colapsar/expandir sidebar (header del sidebar) */
    button[data-testid="stBaseButton-headerNoPadding"] [data-testid="stIconMaterial"] {
        font-size: 0 !important;
        color: transparent !important;
        width: 1.4rem;
        height: 1.4rem;
        display: inline-block;
        position: relative;
    }
    button[data-testid="stBaseButton-headerNoPadding"] [data-testid="stIconMaterial"]::after {
        content: "≡";
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 1.4rem !important;
        color: #bd00ff !important;
        text-shadow: 0 0 8px rgba(189, 0, 255, 0.8);
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
    }

    /* Chevron expand_more del menú de navegación (summary > icon) */
    section[data-testid="stSidebar"] details > summary [data-testid="stIconMaterial"] {
        font-size: 0 !important;
        color: transparent !important;
        width: 1rem;
        height: 1rem;
        display: inline-block;
        position: relative;
    }
    section[data-testid="stSidebar"] details > summary [data-testid="stIconMaterial"]::after {
        content: "▾";
        font-family: 'JetBrains Mono', monospace !important;
        font-size: 0.9rem !important;
        color: #bd00ff !important;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
    }
    /* Cuando el details está abierto, rotamos el chevron */
    section[data-testid="stSidebar"] details[open] > summary [data-testid="stIconMaterial"]::after {
        content: "▴";
    }

    /* Para los demás iconos Material (los de st.Page en el nav, alerts, etc.),
       intentamos hacer que carguen la fuente correcta como fallback. Si no
       carga, al menos los hacemos pequeños para que no estorben. */
    [data-testid="stIconMaterial"]:not(button[data-testid="stBaseButton-headerNoPadding"] *):not(section[data-testid="stSidebar"] details > summary *) {
        font-family: "Material Symbols Rounded", "Material Symbols Outlined" !important;
        font-feature-settings: "liga" !important;
        font-weight: normal !important;
        font-style: normal !important;
        letter-spacing: normal !important;
        text-transform: none !important;
        line-height: 1 !important;
        color: #bd00ff !important;
    }

    h1, h2, h3, h4 {
        font-family: 'Orbitron', 'JetBrains Mono', monospace !important;
        font-weight: 700 !important;
        letter-spacing: 1px;
    }

    h1 {
        color: #bd00ff !important;
        text-shadow:
            0 0 10px rgba(189, 0, 255, 0.8),
            0 0 20px rgba(189, 0, 255, 0.6),
            0 0 30px rgba(189, 0, 255, 0.4);
        border-bottom: 2px solid #bd00ff;
        padding-bottom: 0.6rem;
        box-shadow: 0 4px 20px rgba(189, 0, 255, 0.3);
    }

    h2 {
        color: #00f0ff !important;
        text-shadow:
            0 0 8px rgba(0, 240, 255, 0.7),
            0 0 16px rgba(0, 240, 255, 0.4);
        margin-top: 1.5rem !important;
    }

    h3 {
        color: #ff006e !important;
        text-shadow: 0 0 6px rgba(255, 0, 110, 0.5);
    }

    h4 {
        color: #39ff14 !important;
        text-shadow: 0 0 4px rgba(57, 255, 20, 0.4);
    }

    /* KPI cards */
    .kpi-card {
        background: linear-gradient(135deg, rgba(26, 13, 46, 0.9) 0%, rgba(36, 22, 64, 0.95) 100%);
        border: 1px solid #3d1f6b;
        border-left: 3px solid #bd00ff;
        border-radius: 4px;
        padding: 1.25rem 1.1rem;
        height: 100%;
        position: relative;
        overflow: hidden;
        transition: all 0.2s ease;
    }
    .kpi-card:hover {
        transform: translateY(-3px);
        border-color: #bd00ff;
        box-shadow:
            0 0 20px rgba(189, 0, 255, 0.5),
            inset 0 0 20px rgba(189, 0, 255, 0.1);
    }
    .kpi-card::before {
        content: "";
        position: absolute;
        top: 0; right: 0;
        width: 30px; height: 30px;
        background: linear-gradient(135deg, transparent 50%, rgba(189, 0, 255, 0.3) 100%);
    }
    .kpi-label {
        color: #00f0ff;
        font-size: 0.72rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 2px;
        margin-bottom: 0.5rem;
        text-shadow: 0 0 4px rgba(0, 240, 255, 0.5);
    }
    .kpi-value {
        color: #e8d8ff;
        font-family: 'Orbitron', monospace !important;
        font-size: 2.1rem;
        font-weight: 700;
        line-height: 1.1;
        text-shadow: 0 0 8px rgba(232, 216, 255, 0.5);
    }
    .kpi-sub {
        color: #9d8bb8;
        font-size: 0.75rem;
        margin-top: 0.4rem;
        letter-spacing: 0.5px;
    }
    .kpi-accent { border-left-color: #00f0ff; }
    .kpi-accent .kpi-value { color: #00f0ff; text-shadow: 0 0 12px rgba(0, 240, 255, 0.7); }
    .kpi-magenta { border-left-color: #ff006e; }
    .kpi-magenta .kpi-value { color: #ff006e; text-shadow: 0 0 12px rgba(255, 0, 110, 0.7); }
    .kpi-success { border-left-color: #39ff14; }
    .kpi-success .kpi-value { color: #39ff14; text-shadow: 0 0 12px rgba(57, 255, 20, 0.7); }
    .kpi-warning { border-left-color: #ffb800; }
    .kpi-warning .kpi-value { color: #ffb800; text-shadow: 0 0 12px rgba(255, 184, 0, 0.7); }

    /* Featured paper banner */
    .featured-paper {
        background:
            linear-gradient(90deg, rgba(189, 0, 255, 0.12) 0%, rgba(0, 240, 255, 0.08) 100%);
        border: 1px solid #3d1f6b;
        border-left: 4px solid #bd00ff;
        border-radius: 4px;
        padding: 1.1rem 1.3rem;
        margin: 1rem 0;
        position: relative;
        box-shadow: inset 0 0 30px rgba(189, 0, 255, 0.05);
    }
    .featured-paper::after {
        content: "▸";
        position: absolute;
        top: 1rem; right: 1rem;
        color: #bd00ff;
        font-size: 1.5rem;
        text-shadow: 0 0 8px rgba(189, 0, 255, 0.8);
    }
    .featured-label {
        color: #00f0ff;
        font-size: 0.7rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 3px;
        text-shadow: 0 0 6px rgba(0, 240, 255, 0.6);
    }
    .featured-title {
        color: #e8d8ff;
        font-family: 'Orbitron', monospace;
        font-size: 1.05rem;
        font-weight: 600;
        margin: 0.5rem 0;
        line-height: 1.4;
    }
    .featured-meta {
        color: #9d8bb8;
        font-size: 0.82rem;
        letter-spacing: 0.5px;
    }

    /* Botones */
    .stButton > button {
        background: linear-gradient(135deg, #bd00ff 0%, #7b2cbf 100%);
        color: #0a0612 !important;
        font-family: 'Orbitron', monospace !important;
        font-weight: 700 !important;
        text-transform: uppercase;
        letter-spacing: 2px;
        border: 1px solid #bd00ff;
        border-radius: 4px;
        padding: 0.6rem 1.4rem;
        transition: all 0.2s;
        box-shadow: 0 0 15px rgba(189, 0, 255, 0.4);
    }
    .stButton > button:hover {
        transform: translateY(-2px);
        box-shadow:
            0 0 25px rgba(189, 0, 255, 0.8),
            0 0 50px rgba(189, 0, 255, 0.4);
        background: linear-gradient(135deg, #d030ff 0%, #9040d0 100%);
    }

    /* Inputs en sidebar */
    section[data-testid="stSidebar"] input,
    section[data-testid="stSidebar"] textarea,
    section[data-testid="stSidebar"] [data-baseweb="select"] > div {
        background-color: #0a0612 !important;
        color: #e8d8ff !important;
        border: 1px solid #3d1f6b !important;
        border-radius: 4px !important;
        font-family: 'JetBrains Mono', monospace !important;
    }
    section[data-testid="stSidebar"] input:focus,
    section[data-testid="stSidebar"] textarea:focus {
        border-color: #bd00ff !important;
        box-shadow: 0 0 12px rgba(189, 0, 255, 0.5) !important;
    }

    /* DataFrame */
    .stDataFrame {
        border: 1px solid #3d1f6b;
        border-radius: 4px;
        box-shadow: 0 0 20px rgba(189, 0, 255, 0.15);
    }
    .stDataFrame [data-testid="stDataFrameResizable"] {
        background-color: #1a0d2e !important;
    }

    /* Tabs */
    .stTabs [data-baseweb="tab-list"] {
        background-color: #1a0d2e;
        border: 1px solid #3d1f6b;
        border-radius: 4px;
        padding: 0.3rem;
        gap: 0.3rem;
        display: flex !important;
        width: 100% !important;
    }
    .stTabs [data-baseweb="tab"] {
        color: #9d8bb8 !important;
        font-family: 'JetBrains Mono', monospace !important;
        font-weight: 500 !important;
        background-color: transparent !important;
        border-radius: 3px;
        flex: 1 1 0 !important;       /* todos los tabs ocupan igual ancho */
        text-align: center !important;
        justify-content: center !important;
        padding: 0.6rem 0.5rem !important;
        white-space: nowrap !important;
    }
    /* Centrar el contenido (icono + label) dentro de cada tab */
    .stTabs [data-baseweb="tab"] > div {
        justify-content: center !important;
        width: 100% !important;
    }
    /* Ocultar la línea highlight inferior por defecto de baseweb (queda fea con nuestro fondo) */
    .stTabs [data-baseweb="tab-highlight"],
    .stTabs [data-baseweb="tab-border"] {
        display: none !important;
    }
    .stTabs [aria-selected="true"] {
        background: linear-gradient(135deg, #bd00ff 0%, #7b2cbf 100%) !important;
        color: #0a0612 !important;
        font-weight: 700 !important;
        box-shadow: 0 0 15px rgba(189, 0, 255, 0.5);
    }

    /* Alerts */
    div[data-testid="stAlert"] {
        background-color: #1a0d2e !important;
        border: 1px solid #3d1f6b !important;
        border-left-width: 4px !important;
        border-radius: 4px !important;
    }

    /* Progress bar */
    .stProgress > div > div > div > div {
        background: linear-gradient(90deg, #bd00ff 0%, #00f0ff 100%) !important;
        box-shadow: 0 0 10px rgba(189, 0, 255, 0.6);
    }

    /* Scrollbar */
    ::-webkit-scrollbar { width: 10px; height: 10px; }
    ::-webkit-scrollbar-track { background: #0a0612; }
    ::-webkit-scrollbar-thumb {
        background: linear-gradient(180deg, #bd00ff 0%, #7b2cbf 100%);
        border-radius: 4px;
    }
    ::-webkit-scrollbar-thumb:hover {
        background: #bd00ff;
        box-shadow: 0 0 10px rgba(189, 0, 255, 0.8);
    }

    /* Code */
    code {
        color: #00f0ff !important;
        background-color: #1a0d2e !important;
        border: 1px solid #3d1f6b !important;
        border-radius: 3px !important;
        padding: 0.1rem 0.4rem !important;
        font-family: 'JetBrains Mono', monospace !important;
    }

    /* Caption */
    .stCaption, [data-testid="stCaptionContainer"] {
        color: #9d8bb8 !important;
        font-style: italic;
    }

    /* Línea decorativa */
    .scan-line {
        height: 1px;
        background: linear-gradient(90deg, transparent 0%, #bd00ff 50%, transparent 100%);
        margin: 2rem 0;
        animation: scan 4s ease-in-out infinite;
    }
    @keyframes scan {
        0%, 100% { opacity: 0.3; }
        50% { opacity: 1; box-shadow: 0 0 15px #bd00ff; }
    }

    /* Footer */
    .small-footer {
        color: #9d8bb8;
        font-size: 0.75rem;
        font-family: 'JetBrains Mono', monospace;
        text-align: center;
        margin-top: 2rem;
        padding-top: 1rem;
        border-top: 1px solid #3d1f6b;
        letter-spacing: 1px;
    }
    .small-footer a {
        color: #00f0ff !important;
        text-decoration: none;
    }

    /* Nav del sidebar (st.navigation) */
    section[data-testid="stSidebarNav"] {
        background: transparent;
    }
    section[data-testid="stSidebarNav"] a {
        color: #e8d8ff !important;
        font-family: 'JetBrains Mono', monospace !important;
        border-radius: 4px;
        margin: 2px 0;
        transition: all 0.15s;
        position: relative;
    }
    section[data-testid="stSidebarNav"] a:hover {
        background-color: rgba(189, 0, 255, 0.15) !important;
        color: #bd00ff !important;
    }

    /* === Reemplazar emojis del nav con SVGs ===
       NOTA: El reemplazo lo hace inject_nav_icons_js() porque el CSS de
       Streamlit (Emotion CSS-in-JS) tiene mayor especificidad y gana
       sobre nuestros <style>. La inyección por JS aplica style inline
       directamente, que tiene la prioridad máxima. */
    section[data-testid="stSidebarNav"] a {
        position: relative;
    }

    /* Glow effect en hover para el icono SVG */
    section[data-testid="stSidebarNav"] a:hover [data-testid="stIconEmoji"] {
        filter: drop-shadow(0 0 4px rgba(189, 0, 255, 0.8));
    }

    /* Cyber header con gradient */
    .cyber-header {
        font-family: 'Orbitron', monospace !important;
        font-size: 2.4rem;
        font-weight: 900;
        background: linear-gradient(90deg, #bd00ff 0%, #00f0ff 50%, #ff006e 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        text-align: left;
        margin: 1rem 0 0.3rem 0;
        letter-spacing: 3px;
        text-shadow: 0 0 30px rgba(189, 0, 255, 0.5);
    }
    .cyber-subtitle {
        color: #9d8bb8;
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.9rem;
        letter-spacing: 1px;
        margin-bottom: 2rem;
        border-left: 3px solid #bd00ff;
        padding-left: 1rem;
    }

    /* Status badge */
    .status-badge {
        display: inline-block;
        padding: 0.2rem 0.7rem;
        border-radius: 3px;
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.75rem;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
    }
    .status-badge.online {
        background-color: rgba(57, 255, 20, 0.15);
        color: #39ff14;
        border: 1px solid #39ff14;
    }
</style>
"""


def inject_css():
    st.markdown(CUSTOM_CSS, unsafe_allow_html=True)
    st.markdown(ICON_CSS, unsafe_allow_html=True)
    _inject_nav_icons_js()


_NAV_ICONS_JS = """
<script>
(function() {
    // SVGs inline para cada página, indexados por substring del href.
    // Usamos data: URLs para evitar dependencias externas.
    const SVG_HOME = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><path d='M3 9.5L12 3l9 6.5V20a1 1 0 0 1-1 1h-5v-7h-6v7H4a1 1 0 0 1-1-1V9.5z'/></svg>";
    const SVG_ACTIVITY = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><path d='M22 12h-4l-3 9L9 3l-3 9H2'/></svg>";
    const SVG_TRENDING = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><polyline points='22 7 13.5 15.5 8.5 10.5 2 17'/><polyline points='16 7 22 7 22 13'/></svg>";
    const SVG_DATABASE = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><ellipse cx='12' cy='5' rx='9' ry='3'/><path d='M3 5v6c0 1.66 4 3 9 3s9-1.34 9-3V5'/><path d='M3 11v6c0 1.66 4 3 9 3s9-1.34 9-3v-6'/></svg>";
    const SVG_REFRESH = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><polyline points='23 4 23 10 17 10'/><polyline points='1 20 1 14 7 14'/><path d='M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15'/></svg>";
    const SVG_INFO = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><circle cx='12' cy='12' r='10'/><line x1='12' y1='16' x2='12' y2='12'/><line x1='12' y1='8' x2='12.01' y2='8'/></svg>";
    const SVG_SEARCH = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23bd00ff' stroke-width='1.8' stroke-linecap='round' stroke-linejoin='round'><circle cx='11' cy='11' r='8'/><line x1='21' y1='21' x2='16.65' y2='16.65'/></svg>";

    function getSvgForHref(href) {
        // href es absoluto: http://localhost:8501/indicators
        // Normalizar: tomar pathname
        let path;
        try { path = new URL(href).pathname; } catch(e) { path = href; }
        path = path.replace(/\\/$/, '');  // quitar trailing slash

        if (path === '' || path.endsWith('/home')) return SVG_HOME;
        if (path.endsWith('/indicators')) return SVG_ACTIVITY;
        if (path.endsWith('/visualizations')) return SVG_TRENDING;
        if (path.endsWith('/explore')) return SVG_DATABASE;
        if (path.endsWith('/update_db')) return SVG_REFRESH;
        if (path.endsWith('/about')) return SVG_INFO;
        if (path.endsWith('/search')) return SVG_SEARCH;
        return null;
    }

    function replaceIcons() {
        // Buscar en el parent document (porque Streamlit corre en iframe)
        const doc = window.parent.document;
        const links = doc.querySelectorAll('a[data-testid="stSidebarNavLink"]');
        links.forEach(link => {
            const emoji = link.querySelector('[data-testid="stIconEmoji"]');
            if (!emoji || emoji.dataset.iconReplaced === '1') return;

            const svg = getSvgForHref(link.href);
            if (!svg) return;

            emoji.style.cssText = `
                font-size: 0 !important;
                color: transparent !important;
                text-shadow: none !important;
                width: 22px !important;
                height: 22px !important;
                min-width: 22px !important;
                display: inline-block !important;
                background-image: url("${svg}") !important;
                background-repeat: no-repeat !important;
                background-position: center !important;
                background-size: 20px 20px !important;
                vertical-align: middle !important;
                transition: filter 0.2s !important;
            `;
            emoji.dataset.iconReplaced = '1';
        });
    }

    // Ejecutar repetidamente: al inicio, y observando cambios del DOM
    replaceIcons();
    setTimeout(replaceIcons, 200);
    setTimeout(replaceIcons, 600);
    setTimeout(replaceIcons, 1500);

    // MutationObserver para reaccionar a cambios del nav
    try {
        const doc = window.parent.document;
        const observer = new MutationObserver(() => replaceIcons());
        observer.observe(doc.body, { childList: true, subtree: true });
    } catch(e) {
        console.warn('Could not attach observer:', e);
    }
})();
</script>
"""


def _inject_nav_icons_js():
    """Inyecta el JS que reemplaza los emojis del nav con SVGs vía style inline."""
    import streamlit.components.v1 as components
    components.html(_NAV_ICONS_JS, height=0)


def cyber_header(title: str, subtitle: str = ""):
    html = f'<div class="cyber-header">{title}</div>'
    if subtitle:
        html += f'<div class="cyber-subtitle">{subtitle}</div>'
    st.markdown(html, unsafe_allow_html=True)


def scan_line():
    st.markdown('<div class="scan-line"></div>', unsafe_allow_html=True)


def status_badge(text: str, kind: str = "online"):
    dot = svg("online-dot", size=10, css_class="success")
    return (
        f'<span class="status-badge {kind}" style="display:inline-flex;'
        f'align-items:center;gap:0.4em;">{dot}{text}</span>'
    )



def kpi_card(label: str, value: str, sub: str = "", flavor: str = ""):
    klass = f"kpi-card kpi-{flavor}" if flavor else "kpi-card"
    sub_html = f'<div class="kpi-sub">{sub}</div>' if sub else ""
    st.markdown(
        f'<div class="{klass}"><div class="kpi-label">{label}</div>'
        f'<div class="kpi-value">{value}</div>{sub_html}</div>',
        unsafe_allow_html=True,
    )


def featured_paper(label: str, title: str, meta: str, icon: str = "diamond-filled"):
    """Banner destacado con icono SVG.
    icon: nombre de un icono de icons.py (ej: 'trophy', 'fire', 'diamond-filled')."""
    icon_html = svg(icon, size=18, css_class="accent")
    st.markdown(
        f'<div class="featured-paper">'
        f'<div class="featured-label" style="display:inline-flex;'
        f'align-items:center;gap:0.5em;">{icon_html}{label}</div>'
        f'<div class="featured-title">{title}</div>'
        f'<div class="featured-meta">{meta}</div></div>',
        unsafe_allow_html=True,
    )


def fmt_int(n) -> str:
    if n is None or pd.isna(n):
        return "—"
    return f"{int(n):,}".replace(",", ".")


def fmt_float(n, d=2) -> str:
    if n is None or pd.isna(n):
        return "—"
    return f"{float(n):.{d}f}"


def chart_publications_timeline(df: pd.DataFrame) -> go.Figure:
    if df.empty or df["publication_date"].isna().all():
        return _empty_fig("Sin datos para mostrar")
    s = df.dropna(subset=["publication_date"]).copy()
    s["year_month"] = s["publication_date"].dt.to_period("M").dt.to_timestamp()
    timeline = s.groupby("year_month").size().reset_index(name="count")
    fig = px.line(
        timeline, x="year_month", y="count", markers=True,
        title="◢ EVOLUCIÓN TEMPORAL DE PUBLICACIONES ◣",
    )
    fig.update_traces(
        line=dict(color=COLORS["primary"], width=3),
        marker=dict(size=10, color=COLORS["accent"],
                    line=dict(color=COLORS["primary"], width=2)),
        hovertemplate="<b>%{x|%B %Y}</b><br>%{y} artículos<extra></extra>",
    )
    fig.update_layout(**PLOTLY_THEME["layout"],
                      xaxis_title="MES DE PUBLICACIÓN",
                      yaxis_title="N° ARTÍCULOS", showlegend=False)
    return fig


def chart_topic_distribution(df: pd.DataFrame) -> go.Figure:
    if df.empty:
        return _empty_fig("Sin datos")
    counts = df["topic_label"].value_counts().reset_index()
    counts.columns = ["topic", "count"]
    fig = px.pie(
        counts, values="count", names="topic", hole=0.6,
        title="◢ DISTRIBUCIÓN POR CATEGORÍA ◣",
        color="topic", color_discrete_map=TOPIC_COLORS,
    )
    fig.update_traces(
        textposition="outside", textinfo="label+percent",
        marker=dict(line=dict(color=COLORS["bg"], width=3)),
        textfont=dict(family="JetBrains Mono", color=COLORS["text"], size=12),
    )
    fig.update_layout(**PLOTLY_THEME["layout"], showlegend=False)
    return fig


def chart_top_authors(df: pd.DataFrame, n: int = 10) -> go.Figure:
    if df.empty:
        return _empty_fig("Sin datos")
    authors = df["authors_raw"].fillna("").str.split("; ").explode().str.strip()
    authors = authors[authors != ""]
    top = authors.value_counts().head(n).reset_index()
    top.columns = ["author", "papers"]
    top = top.sort_values("papers")
    fig = px.bar(top, x="papers", y="author", orientation="h",
                 title=f"◢ TOP {n} AUTORES ◣", text="papers")
    fig.update_traces(
        marker=dict(
            color=top["papers"],
            colorscale=[[0, COLORS["glow"]], [0.5, COLORS["primary"]], [1, COLORS["accent"]]],
            line=dict(color=COLORS["accent"], width=1),
        ),
        textposition="outside",
        textfont=dict(color=COLORS["accent"], family="JetBrains Mono"),
        hovertemplate="<b>%{y}</b><br>%{x} artículos<extra></extra>",
    )
    fig.update_layout(**PLOTLY_THEME["layout"],
                      xaxis_title="N° ARTÍCULOS", yaxis_title="",
                      showlegend=False, height=440)
    return fig


def chart_citations_histogram(df: pd.DataFrame) -> go.Figure:
    if df.empty or df["citations"].dropna().empty:
        return _empty_fig("Sin datos de citas")
    fig = px.histogram(
        df.dropna(subset=["citations"]), x="citations",
        nbins=30, title="◢ DISTRIBUCIÓN DE CITAS ◣",
    )
    fig.update_traces(
        marker=dict(color=COLORS["neon_green"],
                    line=dict(color=COLORS["bg"], width=1), opacity=0.85),
        hovertemplate="<b>%{x} citas</b><br>%{y} artículos<extra></extra>",
    )
    fig.update_layout(**PLOTLY_THEME["layout"],
                      xaxis_title="N° CITAS", yaxis_title="N° ARTÍCULOS",
                      showlegend=False, bargap=0.08)
    return fig


def chart_downloads_vs_citations(df: pd.DataFrame) -> go.Figure:
    if df.empty:
        return _empty_fig("Sin datos")
    plot_df = df.dropna(subset=["citations", "downloads"]).copy()
    if plot_df.empty:
        return _empty_fig("Sin datos para scatter")
    fig = px.scatter(
        plot_df, x="downloads", y="citations", color="topic_label",
        hover_data={"title": True, "downloads": ":,", "citations": True,
                    "topic_label": False},
        color_discrete_map=TOPIC_COLORS,
        title="◢ CITAS vs DESCARGAS ◣",
    )
    fig.update_traces(marker=dict(size=12, opacity=0.85,
                                   line=dict(width=1.5, color=COLORS["text"])))
    fig.update_layout(**PLOTLY_THEME["layout"],
                      xaxis_title="DESCARGAS (ACCESSES)", yaxis_title="CITAS",
                      legend_title_text="CATEGORÍA")
    return fig


def chart_downloads_by_topic(df: pd.DataFrame) -> go.Figure:
    if df.empty:
        return _empty_fig("Sin datos")
    g = df.groupby("topic_label")["downloads"].sum().reset_index()
    g = g.sort_values("downloads", ascending=True)
    fig = px.bar(g, x="downloads", y="topic_label", orientation="h",
                 title="◢ DESCARGAS POR CATEGORÍA ◣",
                 text="downloads", color="topic_label",
                 color_discrete_map=TOPIC_COLORS)
    fig.update_traces(
        texttemplate="%{text:,.0f}", textposition="outside",
        textfont=dict(color=COLORS["text"], family="JetBrains Mono"),
        marker=dict(line=dict(color=COLORS["bg"], width=2)),
        hovertemplate="<b>%{y}</b><br>%{x:,.0f} descargas<extra></extra>",
    )
    fig.update_layout(**PLOTLY_THEME["layout"],
                      xaxis_title="DESCARGAS TOTALES", yaxis_title="",
                      showlegend=False, height=320)
    return fig


def _empty_fig(msg: str) -> go.Figure:
    fig = go.Figure()
    fig.add_annotation(
        text=f"// {msg.upper()} //", xref="paper", yref="paper",
        x=0.5, y=0.5, showarrow=False,
        font=dict(color=COLORS["text_muted"], size=14, family="JetBrains Mono"),
    )
    fig.update_layout(**PLOTLY_THEME["layout"])
    return fig
