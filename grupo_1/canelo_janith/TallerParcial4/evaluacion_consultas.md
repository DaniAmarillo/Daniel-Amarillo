# Evaluación del buscador — Taller 4

Corpus: 204 artículos de *ACM Transactions on Graphics* (base `tog_q1_2025.sqlite`).

Estrategias: **BM25** (léxica), **LSA** = TF-IDF + Truncated SVD (2943 → 40 dimensiones, semántica reducida), **TF-IDF** completo (referencia sin reducción) e **híbrido** BM25+LSA por RRF.

Juicios de relevancia binarios, asignados manualmente sobre el pool de los 10 primeros resultados de las cuatro estrategias.

## Resumen (promedio sobre las 6 consultas)

| estrategia   |    P@5 |    MRR |   nDCG@5 |   Recall@10 |     ms |
|:-------------|-------:|-------:|---------:|------------:|-------:|
| bm25         | 0.4333 | 0.6111 |   0.4435 |      0.6138 | 0.505  |
| lsa          | 0.5    | 0.7685 |   0.5347 |      0.6681 | 0.7233 |
| tfidf        | 0.4667 | 0.625  |   0.4742 |      0.593  | 0.6133 |
| hibrido      | 0.5    | 0.6667 |   0.5074 |      0.7348 | 1.0067 |

## C1 · Términos literales del corpus

**Consulta:** «hexahedral mesh extraction»

**Criterio de relevancia:** Relevante: el aporte central del artículo es generar, extraer o simplificar mallas (volumétricas, cuadriláteras o triangulares). No relevante: artículos que solo consumen mallas ya construidas (renderizado, parametrización, operaciones booleanas).

**Artículos juzgados relevantes (paper_id):** 18, 19, 28, 78, 149, 154, 175

| estrategia   |   P@5 |   MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|------:|---------:|------------:|-----:|
| bm25         |   0.6 |     1 | 0.684352 |    0.857143 | 1.2  |
| lsa          |   0.6 |     1 | 0.684352 |    0.857143 | 1.08 |
| tfidf        |   0.6 |     1 | 0.699215 |    0.857143 | 0.69 |
| hibrido      |   0.6 |     1 | 0.684352 |    1        | 1.12 |

*BM25 (léxica)* — primeros 5:

1. ✔ `[18]` HexHex: Highspeed Extraction of Hexahedral Meshes — puntaje 21.38679
2. ✔ `[19]` TetWeave: Isosurface Extraction using On-The-Fly Delaunay Tetrahedral Grids for Gradient-Based Mesh Optimization — puntaje 10.65631
3. ✘ `[132]` Direct Rendering of Intrinsic Triangulations — puntaje 7.95362
4. ✘ `[12]` Dynamic Mesh Processing on the GPU — puntaje 4.24629
5. ✔ `[175]` IMLS-Splatting: Efficient Mesh Reconstruction from Multi-view Images via Point Representation — puntaje 4.10825

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✔ `[18]` HexHex: Highspeed Extraction of Hexahedral Meshes — puntaje 0.79219
2. ✔ `[19]` TetWeave: Isosurface Extraction using On-The-Fly Delaunay Tetrahedral Grids for Gradient-Based Mesh Optimization — puntaje 0.78717
3. ✘ `[88]` Exact Predicates, Exact Constructions and Combinatorics for Mesh CSG. — puntaje 0.6294
4. ✘ `[132]` Direct Rendering of Intrinsic Triangulations — puntaje 0.6227
5. ✔ `[78]` Simplifying Textured Triangle Meshes in the Wild — puntaje 0.54387

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✔ `[18]` HexHex: Highspeed Extraction of Hexahedral Meshes — puntaje 0.31738
2. ✔ `[19]` TetWeave: Isosurface Extraction using On-The-Fly Delaunay Tetrahedral Grids for Gradient-Based Mesh Optimization — puntaje 0.26237
3. ✘ `[132]` Direct Rendering of Intrinsic Triangulations — puntaje 0.19839
4. ✔ `[175]` IMLS-Splatting: Efficient Mesh Reconstruction from Multi-view Images via Point Representation — puntaje 0.10972
5. ✘ `[12]` Dynamic Mesh Processing on the GPU — puntaje 0.09794

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✔ `[18]` HexHex: Highspeed Extraction of Hexahedral Meshes — puntaje 0.03279
2. ✔ `[19]` TetWeave: Isosurface Extraction using On-The-Fly Delaunay Tetrahedral Grids for Gradient-Based Mesh Optimization — puntaje 0.03226
3. ✘ `[132]` Direct Rendering of Intrinsic Triangulations — puntaje 0.0315
4. ✘ `[88]` Exact Predicates, Exact Constructions and Combinatorics for Mesh CSG. — puntaje 0.03037
5. ✔ `[175]` IMLS-Splatting: Efficient Mesh Reconstruction from Multi-view Images via Point Representation — puntaje 0.03031

## C2 · Términos relacionados / sinónimos

**Consulta:** «teaching virtual characters to move using learned controllers»

**Criterio de relevancia:** Relevante: control o animación de personajes/agentes simulados mediante políticas o modelos aprendidos. No relevante: controladores de otros dominios (drones, robots de manipulación) y sistemas interactivos sin política aprendida. Ninguno de los términos de la consulta («teaching», «learned controllers») aparece literalmente en los artículos objetivo, que usan «skills», «policy», «control».

**Artículos juzgados relevantes (paper_id):** 30, 31, 41, 72, 168, 200

| estrategia   |   P@5 |      MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|---------:|---------:|------------:|-----:|
| bm25         |   0.4 | 0.25     | 0.277273 |    0.5      | 0.42 |
| lsa          |   0.6 | 1        | 0.654809 |    0.666667 | 0.66 |
| tfidf        |   0.6 | 0.333333 | 0.446854 |    0.5      | 0.57 |
| hibrido      |   0.4 | 0.333333 | 0.315648 |    0.833333 | 1.03 |

*BM25 (léxica)* — primeros 5:

1. ✘ `[179]` VR-Doh: Hands-on 3D Modeling in Virtual Reality — puntaje 8.35107
2. ✘ `[71]` A Highly-Efficient Hybrid Simulation System for Flight Controller Design and Evaluation of Unmanned Aerial Vehicles — puntaje 8.32169
3. ✘ `[177]` A Deep Learning-based Virtual Oculoplastic Surgery Simulator — puntaje 5.48202
4. ✔ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 5.45372
5. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 5.09661

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.70738
2. ✘ `[177]` A Deep Learning-based Virtual Oculoplastic Surgery Simulator — puntaje 0.65593
3. ✔ `[30]` PhysicsFC: Learning User-Controlled Skills for a Physics-Based Football Player Controller — puntaje 0.61515
4. ✔ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 0.58141
5. ✘ `[179]` VR-Doh: Hands-on 3D Modeling in Virtual Reality — puntaje 0.51911

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✘ `[179]` VR-Doh: Hands-on 3D Modeling in Virtual Reality — puntaje 0.16569
2. ✘ `[177]` A Deep Learning-based Virtual Oculoplastic Surgery Simulator — puntaje 0.12896
3. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.12431
4. ✔ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 0.10221
5. ✔ `[72]` CFC: Simulating Character-Fluid Coupling using a Two-Level World Model — puntaje 0.09354

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✘ `[177]` A Deep Learning-based Virtual Oculoplastic Surgery Simulator — puntaje 0.032
2. ✘ `[179]` VR-Doh: Hands-on 3D Modeling in Virtual Reality — puntaje 0.03178
3. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.03178
4. ✔ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 0.03125
5. ✘ `[71]` A Highly-Efficient Hybrid Simulation System for Flight Controller Design and Evaluation of Unmanned Aerial Vehicles — puntaje 0.03062

## C3 · Consulta general

**Consulta:** «physics-based simulation»

**Criterio de relevancia:** Relevante: el artículo propone o mejora un método de simulación física (sólidos, fluidos, granulares, colisiones, integradores) o un controlador que opera sobre una simulación física.

**Artículos juzgados relevantes (paper_id):** 24, 25, 30, 31, 41, 54, 107, 140, 155, 200, 203

| estrategia   |   P@5 |   MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|------:|---------:|------------:|-----:|
| bm25         |   1   |     1 | 1        |    0.909091 | 0.38 |
| lsa          |   0.8 |     1 | 0.786014 |    0.818182 | 0.64 |
| tfidf        |   1   |     1 | 1        |    0.909091 | 0.52 |
| hibrido      |   1   |     1 | 1        |    0.909091 | 0.96 |

*BM25 (léxica)* — primeros 5:

1. ✔ `[24]` Arenite: A Physics-based Sandstone Simulator — puntaje 7.78542
2. ✔ `[107]` Adaptive Phase-Field-FLIP for Very Large Scale Two-Phase Fluid Simulation — puntaje 5.86491
3. ✔ `[140]` Neurally Integrated Finite Elements for Differentiable Elasticity on Evolving Domains — puntaje 5.32046
4. ✔ `[203]` Reliable Iterative Dynamics: A Versatile Method for Fast and Robust Simulation — puntaje 4.98736
5. ✔ `[30]` PhysicsFC: Learning User-Controlled Skills for a Physics-Based Football Player Controller — puntaje 4.98683

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✔ `[24]` Arenite: A Physics-based Sandstone Simulator — puntaje 0.77083
2. ✘ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 0.64774
3. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.55798
4. ✔ `[30]` PhysicsFC: Learning User-Controlled Skills for a Physics-Based Football Player Controller — puntaje 0.54307
5. ✔ `[25]` CK-MPM: A Compact-Kernel Material Point Method — puntaje 0.52042

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✔ `[24]` Arenite: A Physics-based Sandstone Simulator — puntaje 0.22734
2. ✔ `[107]` Adaptive Phase-Field-FLIP for Very Large Scale Two-Phase Fluid Simulation — puntaje 0.1976
3. ✔ `[54]` C5D: Sequential Continuous Convex Collision Detection Using Cone Casting — puntaje 0.15156
4. ✔ `[155]` Augmented Vertex Block Descent — puntaje 0.13115
5. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.1203

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✔ `[24]` Arenite: A Physics-based Sandstone Simulator — puntaje 0.03279
2. ✔ `[107]` Adaptive Phase-Field-FLIP for Very Large Scale Two-Phase Fluid Simulation — puntaje 0.03105
3. ✔ `[30]` PhysicsFC: Learning User-Controlled Skills for a Physics-Based Football Player Controller — puntaje 0.03101
4. ✔ `[140]` Neurally Integrated Finite Elements for Differentiable Elasticity on Evolving Domains — puntaje 0.03058
5. ✔ `[31]` ViSA: Physics-based Virtual Stunt Actors for Ballistic Stunts — puntaje 0.03058

## C4 · Consulta específica

**Consulta:** «bounding volumes for signed distance fields»

**Criterio de relevancia:** Relevante: representaciones implícitas (SDF, superficies neuronales implícitas, level sets) y las estructuras que acotan o aceleran su trazado. No relevante: mallas explícitas, splatting y aplicaciones donde la representación implícita es solo un medio (p. ej. planeación de impresión 3D).

**Artículos juzgados relevantes (paper_id):** 6, 13, 59, 61, 198

| estrategia   |   P@5 |   MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|------:|---------:|------------:|-----:|
| bm25         |   0.4 |     1 | 0.553146 |           1 | 0.36 |
| lsa          |   0.6 |     1 | 0.699215 |           1 | 0.59 |
| tfidf        |   0.4 |     1 | 0.553146 |           1 | 0.83 |
| hibrido      |   0.6 |     1 | 0.699215 |           1 | 1.06 |

*BM25 (léxica)* — primeros 5:

1. ✔ `[13]` Sphere Carving: Bounding Volumes for Signed Distance Fields — puntaje 30.31254
2. ✔ `[6]` Synchronized Tracing of Primitive-based Implicit Volumes — puntaje 20.46357
3. ✘ `[68]` INF-3DP: Implicit Neural Fields for Collision-Free Multi-Axis 3D Printing — puntaje 9.98906
4. ✘ `[130]` Don't Splat your Gaussians: Volumetric Ray-Traced Primitives for Modeling and Rendering Scattering and Emissive Media — puntaje 8.79532
5. ✘ `[161]` CAST: Component-Aligned 3D Scene Reconstruction from an RGB Image — puntaje 5.92318

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✔ `[13]` Sphere Carving: Bounding Volumes for Signed Distance Fields — puntaje 0.9684
2. ✔ `[6]` Synchronized Tracing of Primitive-based Implicit Volumes — puntaje 0.91646
3. ✘ `[68]` INF-3DP: Implicit Neural Fields for Collision-Free Multi-Axis 3D Printing — puntaje 0.44908
4. ✔ `[198]` A Neural Particle Level Set Method for Dynamic Interface Tracking — puntaje 0.38402
5. ✘ `[87]` NESI: Neural Explicit-Shape-Intersection-Based Geometry Representation — puntaje 0.33491

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✔ `[13]` Sphere Carving: Bounding Volumes for Signed Distance Fields — puntaje 0.55897
2. ✔ `[6]` Synchronized Tracing of Primitive-based Implicit Volumes — puntaje 0.3378
3. ✘ `[68]` INF-3DP: Implicit Neural Fields for Collision-Free Multi-Axis 3D Printing — puntaje 0.17067
4. ✘ `[161]` CAST: Component-Aligned 3D Scene Reconstruction from an RGB Image — puntaje 0.1152
5. ✘ `[130]` Don't Splat your Gaussians: Volumetric Ray-Traced Primitives for Modeling and Rendering Scattering and Emissive Media — puntaje 0.09376

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✔ `[13]` Sphere Carving: Bounding Volumes for Signed Distance Fields — puntaje 0.03279
2. ✔ `[6]` Synchronized Tracing of Primitive-based Implicit Volumes — puntaje 0.03226
3. ✘ `[68]` INF-3DP: Implicit Neural Fields for Collision-Free Multi-Axis 3D Printing — puntaje 0.03175
4. ✔ `[198]` A Neural Particle Level Set Method for Dynamic Interface Tracking — puntaje 0.02991
5. ✘ `[130]` Don't Splat your Gaussians: Volumetric Ray-Traced Primitives for Modeling and Rendering Scattering and Emissive Media — puntaje 0.02951

## C5 · Consulta donde se esperan resultados poco relevantes

**Consulta:** «simulating how clothes wrinkle and fold on a moving body»

**Criterio de relevancia:** Relevante: simulación de telas, prendas o tejidos deformables. No relevante: dinámica de cuerpos rígidos, fluidos o granulares. La consulta está redactada con vocabulario coloquial («clothes», «body») y «body» es polisémico en este corpus: designa cuerpos rígidos, no el cuerpo humano.

**Artículos juzgados relevantes (paper_id):** 20, 116, 117, 118, 184, 204

| estrategia   |   P@5 |      MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|---------:|---------:|------------:|-----:|
| bm25         |     0 | 0.166667 |        0 |    0.166667 | 0.35 |
| lsa          |     0 | 0.111111 |        0 |    0.166667 | 0.7  |
| tfidf        |     0 | 0.166667 |        0 |    0.166667 | 0.55 |
| hibrido      |     0 | 0.166667 |        0 |    0.166667 | 0.94 |

*BM25 (léxica)* — primeros 5:

1. ✘ `[72]` CFC: Simulating Character-Fluid Coupling using a Two-Level World Model — puntaje 8.86097
2. ✘ `[22]` Controllable Complex Freezing Dynamics Simulation on Thin Films — puntaje 6.27454
3. ✘ `[53]` A Versatile Quaternion-Based Constrained Rigid Body Dynamics — puntaje 5.63307
4. ✘ `[51]` Putting Rigid Bodies to Rest — puntaje 5.54511
5. ✘ `[203]` Reliable Iterative Dynamics: A Versatile Method for Fast and Robust Simulation — puntaje 5.17237

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✘ `[72]` CFC: Simulating Character-Fluid Coupling using a Two-Level World Model — puntaje 0.83017
2. ✘ `[53]` A Versatile Quaternion-Based Constrained Rigid Body Dynamics — puntaje 0.752
3. ✘ `[155]` Augmented Vertex Block Descent — puntaje 0.68179
4. ✘ `[51]` Putting Rigid Bodies to Rest — puntaje 0.61386
5. ✘ `[22]` Controllable Complex Freezing Dynamics Simulation on Thin Films — puntaje 0.58813

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✘ `[72]` CFC: Simulating Character-Fluid Coupling using a Two-Level World Model — puntaje 0.18406
2. ✘ `[51]` Putting Rigid Bodies to Rest — puntaje 0.12736
3. ✘ `[53]` A Versatile Quaternion-Based Constrained Rigid Body Dynamics — puntaje 0.1142
4. ✘ `[22]` Controllable Complex Freezing Dynamics Simulation on Thin Films — puntaje 0.11314
5. ✘ `[9]` Inverse Geometric Locomotion — puntaje 0.09181

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✘ `[72]` CFC: Simulating Character-Fluid Coupling using a Two-Level World Model — puntaje 0.03279
2. ✘ `[53]` A Versatile Quaternion-Based Constrained Rigid Body Dynamics — puntaje 0.032
3. ✘ `[22]` Controllable Complex Freezing Dynamics Simulation on Thin Films — puntaje 0.03151
4. ✘ `[51]` Putting Rigid Bodies to Rest — puntaje 0.03125
5. ✘ `[203]` Reliable Iterative Dynamics: A Versatile Method for Fast and Robust Simulation — puntaje 0.03031

## C6 · Consulta en lenguaje natural (estilo enunciado)

**Consulta:** «applications of generative artificial intelligence for creating 3D content»

**Criterio de relevancia:** Relevante: modelos generativos aplicados a la creación o edición de contenido tridimensional (mallas, texturas, materiales, escenas, rigging). No relevante: generación o edición puramente 2D (imágenes, diseño gráfico, retoque), aunque también use modelos de difusión.

**Artículos juzgados relevantes (paper_id):** 29, 43, 74, 82, 125, 151, 161, 184

| estrategia   |   P@5 |   MRR |   nDCG@5 |   Recall@10 |   ms |
|:-------------|------:|------:|---------:|------------:|-----:|
| bm25         |   0.2 |  0.25 | 0.146068 |       0.25  | 0.32 |
| lsa          |   0.4 |  0.5  | 0.383566 |       0.5   | 0.67 |
| tfidf        |   0.2 |  0.25 | 0.146068 |       0.125 | 0.52 |
| hibrido      |   0.4 |  0.5  | 0.345191 |       0.5   | 0.93 |

*BM25 (léxica)* — primeros 5:

1. ✘ `[86]` Noise-Coded Illumination for Forensic and Photometric Video Analysis — puntaje 8.89013
2. ✘ `[141]` B4M: Breaking Low-Rank Adapter for Making Content-Style Customization — puntaje 7.24335
3. ✘ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 6.76747
4. ✔ `[151]` BANG: Dividing 3D Assets via Generative Exploded Dynamics — puntaje 5.99328
5. ✘ `[56]` Example-Based Feature Painting on Textures — puntaje 5.81809

*LSA / TF-IDF + SVD (semántica, reducida)* — primeros 5:

1. ✘ `[106]` TokenVerse: Versatile Multi-concept Personalization in Token Modulation Space — puntaje 0.56889
2. ✔ `[151]` BANG: Dividing 3D Assets via Generative Exploded Dynamics — puntaje 0.5541
3. ✔ `[161]` CAST: Component-Aligned 3D Scene Reconstruction from an RGB Image — puntaje 0.5513
4. ✘ `[102]` IntrinsicEdit: Precise generative image manipulation in intrinsic space — puntaje 0.54067
5. ✘ `[86]` Noise-Coded Illumination for Forensic and Photometric Video Analysis — puntaje 0.53753

*TF-IDF completo + coseno (léxica vectorial)* — primeros 5:

1. ✘ `[86]` Noise-Coded Illumination for Forensic and Photometric Video Analysis — puntaje 0.18846
2. ✘ `[141]` B4M: Breaking Low-Rank Adapter for Making Content-Style Customization — puntaje 0.15931
3. ✘ `[168]` Creating Fluid-Interactive Virtual Agents by an Efficient Simulator with Local-domain Control — puntaje 0.11125
4. ✔ `[151]` BANG: Dividing 3D Assets via Generative Exploded Dynamics — puntaje 0.10729
5. ✘ `[62]` Generative Head-Mounted Camera Captures for Photorealistic Avatars — puntaje 0.09069

*Híbrido BM25 + LSA (RRF)* — primeros 5:

1. ✘ `[86]` Noise-Coded Illumination for Forensic and Photometric Video Analysis — puntaje 0.03178
2. ✔ `[151]` BANG: Dividing 3D Assets via Generative Exploded Dynamics — puntaje 0.03175
3. ✘ `[141]` B4M: Breaking Low-Rank Adapter for Making Content-Style Customization — puntaje 0.03105
4. ✘ `[102]` IntrinsicEdit: Precise generative image manipulation in intrinsic space — puntaje 0.03055
5. ✔ `[161]` CAST: Component-Aligned 3D Scene Reconstruction from an RGB Image — puntaje 0.02996
