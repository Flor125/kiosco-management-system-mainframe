# Small Business Management: Hybrid Kiosk System on IBM LinuxONE

[![Status](https://img.shields.io/badge/Status-Completed-success)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Architecture](https://img.shields.io/badge/Architecture-Hybrid%20(Web%20%2B%20Mainframe)-blue)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Platform](https://img.shields.io/badge/Platform-IBM%20LinuxONE%20(s390x)-purple)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Tech](https://img.shields.io/badge/Stack-Python%20|%20Flask%20|%20COBOL%20|%20Db2%20|%20Docker-blueviolet)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Grade](https://img.shields.io/badge/Grade-9%2F10-brightgreen)](https://github.com/Flor125/kiosco-management-system-mainframe)

> **🏆 Academic Context:** Only project from the 2024-2025 cohort fully implemented, deployed, and operational (vs theoretical proposals from other students). Developed throughout 2024-2025, defended December 16, 2025 with grade 9/10 at Universidad Nacional de Villa Mercedes.
> 
> 📄 **Complete Academic Thesis:** [View PDF (56 pages)](./thesis_document/TESIS.pdf) - Includes UML diagrams, use cases, detailed architecture, and operation logs.

---

## 📋 Overview

Inventory and sales management system for retail kiosks deployed on **IBM LinuxONE (s390x mainframe architecture)**. It combines the agility of a modern web interface with the robustness of legacy batch processing, demonstrating real-world integration of enterprise mainframe systems with contemporary technologies.

**Key Achievement:** Real production deployment on IBM enterprise infrastructure handling actual internet traffic and security incidents, not a desktop simulation.

---

## 🚀 Hybrid Architecture

The system manages inventory using two paradigms:

1. **Online (Real-time):** Python + Flask handle the mobile interface, barcode scanning, and direct transactions to the containerized **IBM Db2** database.
2. **Batch (Back-office):** A pure **COBOL** module processes large volumes of data for average cost calculation, expiration detection, and historical data generation.

### 🔄 Integration Flow (The "Zero SQL" Approach)

Due to compilation constraints in the s390x mainframe environment (GnuCOBOL without Enterprise precompiler), an integration based on flat file exchange was implemented, simulating classic mainframe batch processing:

1. **Extraction:** Python extracts data from Db2 and generates `costos.dat` and `vencimientos.dat` (sequential files).
2. **Processing:** The COBOL program (`batchcosto.cbl`) reads the sequential files, applies business logic (profit rules, FEFO alerts), and generates `historico.dat` and `alertas.dat`.
3. **Load:** Python reinjects the processed results into Db2 for frontend visualization.

**Why this approach?** GnuCOBOL on s390x lacks native EXEC SQL support, requiring a workaround that actually mirrors real-world mainframe-to-modern integration patterns used in banking and insurance.

---

## ✨ Key Features

* **Mobile Scanning:** Real-time price and stock lookup using mobile camera (QuaggaJS barcode reader).
* **FEFO Logic (First Expired, First Out):** System automatically deducts the batch with nearest expiration date when processing sales.
* **Smart Alerts:** Automatic detection of products nearing expiration with "Combo Offer" suggestions to clear inventory.
* **Batch Processing:** Overnight COBOL jobs calculate weighted average costs and generate business intelligence reports.
* **Enterprise Infrastructure:** Deployed on **IBM LinuxONE Community Cloud** with Db2 in Docker containers.
* **Production-Ready Security:** Session management, password hashing, audit logs, Docker isolation.

---

## 🛠 Tech Stack

* **Frontend:** HTML5, CSS3, JavaScript (QuaggaJS for barcode scanning).
* **Backend API:** Python 3.10, Flask, ibm_db driver
* **Batch Processing:** COBOL (GnuCOBOL for s390x)
* **Database:** IBM Db2 Community Edition (Dockerized on s390x)
* **Platform:** IBM LinuxONE Community Cloud (s390x mainframe architecture)
* **OS:** Ubuntu Server 20.04 for s390x
* **Security:** pbkdf2:sha256 password hashing, session validation, CSRF protection, RBAC

---

## 📸 Screenshots

*Mobile-first* design optimized for kiosk operators.

| Access & Security | Main Dashboard |
|:---:|:---:|
| <img src="./screenshots_thesis/login.png" width="300"> | <img src="./screenshots_thesis/system1.png" width="300"> |
| **Secure Login & Registration** | **Dashboard & Alerts** |

| Stock Management (FEFO) | Purchase Registration |
|:---:|:---:|
| <img src="./screenshots_thesis/system4.png" width="300"> | <img src="./screenshots_thesis/system2.png" width="300"> |
| **Batch & Expiration View** | **Stock Entry & Scanner** |

---

## 🏗️ Deployment Architecture

**IMPORTANT:** This project was deployed on **IBM LinuxONE Community Cloud** (s390x mainframe architecture), not on standard x86_64 servers.

### Infrastructure Requirements
- **Platform:** IBM LinuxONE Community Cloud (s390x mainframe architecture)
- **OS:** Ubuntu Server 20.04 for s390x
- **Database:** IBM Db2 Community Edition (Docker container for s390x)
- **COBOL Compiler:** GnuCOBOL compiled for s390x (limited EXEC SQL support)
- **Python:** 3.10+ with ibm_db driver for s390x
- **Network:** Public IP exposure for production testing

### Why This Deployment is Unique

1. **True mainframe environment:** Not a simulation or x86_64 emulation - real IBM enterprise infrastructure
2. **Architecture constraints:** s390x-specific binaries, limited tooling compared to x86_64 development environments
3. **Production-grade setup:** Exposed to internet, handling real traffic and security threats
4. **Integration challenges:** GnuCOBOL on s390x lacks EXEC SQL precompiler, requiring flat-file integration pattern
5. **Enterprise database:** IBM Db2 running in mainframe environment, not SQLite or MySQL

### Project Structure (Hybrid)
```
kiosco-management-system-mainframe/
├── app.py                  # Flask web application (Python)
├── cobol/
│   ├── batchcosto.cbl     # Batch processing logic (COBOL)
│   ├── costos.dat         # Input: Purchase data for batch
│   ├── historico.dat      # Output: Calculated averages
│   └── alertas.dat        # Output: Expiration alerts
├── data/                   # Database initialization scripts
├── templates/              # HTML templates (mobile-first)
├── static/                 # CSS, JavaScript, assets
├── screenshots_thesis/     # Evidence of deployment
└── thesis_document/        # Complete academic thesis (56 pages)
```

This structure demonstrates the **integration of two worlds**: the `cobol/` folder contains batch logic and exchange files, coexisting with the Flask application.

![VS Code Structure](./screenshots_thesis/vscode_structure.png)

### Replication Notes

This project **cannot be easily replicated** on standard development machines (x86_64). It requires:
- Access to IBM LinuxONE Community Cloud (free tier available for developers at [linuxone.cloud.marist.edu](https://linuxone.cloud.marist.edu))
- Understanding of s390x-specific compilation and deployment
- Configuration of Db2 for mainframe architecture
- Experience with mainframe batch processing patterns

**For recruiters/reviewers:** Complete operation logs, screenshots, and academic thesis (56 pages) provide comprehensive evidence of deployment and functionality.

---

## 🛡️ Production Deployment & Security

### Real-World Operation

**System was deployed on IBM LinuxONE (s390x) and exposed to the internet** for testing, demonstration, and academic evaluation.

**Security incidents documented during operation:**
- ✅ Multiple automated bot attacks detected and blocked
- ✅ Unauthorized access attempts logged and rejected  
- ✅ Session hijacking attempts prevented
- ✅ SQL injection patterns filtered
- ✅ Brute force login attempts rate-limited

**Security implementation:**
- Password hashing: pbkdf2:sha256 with salt
- Session validation and timeout management
- CSRF protection on all POST endpoints
- Role-based access control (MANAGER/EMPLOYEE roles)
- Docker container isolation on mainframe platform
- Complete audit trail of all operations
- Input validation and parameterized queries

![Server Logs](./screenshots_thesis/logs.png)

*Evidence: Production logs showing real traffic, security incidents, and system stability on mainframe infrastructure*

---

## 🎯 Why This Project Matters

### Business Problem

Small retail stores (kiosks) in Argentina face:
- Manual inventory control → Human errors in stock counts
- Lack of expiration tracking → Product waste and losses
- No batch-level traceability → Compliance and audit issues
- Complex pricing calculations → Lost revenue opportunities
- Inefficient stock rotation → Capital tied up in old inventory

### Technical Challenge

Build an enterprise-grade system on **mainframe infrastructure** (IBM LinuxONE s390x) that:
- Handles real-time web transactions with responsive UI
- Processes batch jobs overnight (COBOL) for heavy computations
- Integrates legacy and modern technologies seamlessly
- Operates under compilation constraints (GnuCOBOL without EXEC SQL)
- Maintains ACID transactions and data consistency
- Runs in production with real security threats

### Solution Impact

**For operators:**
- Mobile-first interface accessible from any smartphone
- Barcode scanning for instant product lookup
- Real-time stock visibility by batch and expiration date
- Automated alerts prevent expired product sales

**For business:**
- FEFO logic minimizes waste (First Expired, First Out)
- Smart pricing with weighted average cost calculation
- Inventory turnover analysis and optimization
- Combo/offer suggestions based on expiration patterns

**For developers:**
- Demonstrates hybrid architecture patterns (web + batch)
- Shows real mainframe deployment (not simulation)
- Proves legacy-modern integration feasibility
- Provides production security implementation example

### Technical Achievement

**Beyond Standard Web Development:**
- ✅ Deployed on **real IBM mainframe infrastructure** (not x86_64 simulation)
- ✅ Handled **s390x architecture constraints** (limited tooling, different binaries)
- ✅ Solved **GnuCOBOL limitations** on s390x (no EXEC SQL → flat-file integration)
- ✅ Managed **enterprise-grade database** (Db2) in mainframe environment
- ✅ Operated in **production** with real internet traffic and security incidents

This demonstrates experience with:
- True enterprise mainframe systems (not just COBOL on desktop)
- Legacy-modern integration patterns used in banking/insurance
- Resource-constrained problem solving
- Production deployment and operations
- Security incident handling and logging

---

## 👤 Author

**Florencia Alicia Sombra**

### Education
🎓 Associate Degree in Systems Programming  
Universidad Nacional de Villa Mercedes (2022-2026)

### Certifications
- 📜 **IBM Mainframe Developer Professional Certificate** (2024-2025)
- 📜 **IBM z/OS Mainframe Practitioner Professional Certificate** (2024-2025)
- 📜 **IBM Z Xplore** - Concepts, Advanced, All Star (2024)
- 📜 **Fundamentals for Zowe** - Interskill Learning (2025)
- 📜 **Coaching and Mentoring for Technical Specialists** - Interskill Learning (2025)

### Technical Skills
**Mainframe:** COBOL, JCL, REXX, z/OS, TSO, VSAM, Db2, Zowe, z/OSMF, Ansible  
**Backend:** Python, Flask, REST APIs, Batch Processing  
**Infrastructure:** Docker, Linux, IBM LinuxONE (s390x), Git/GitHub

### Contact & Links
- 💼 **LinkedIn:** [linkedin.com/in/flor125](https://linkedin.com/in/flor125)
- 💻 **GitHub:** [github.com/Flor125](https://github.com/Flor125)
- 📧 **Email:** sombraflorencia097@gmail.com
- 🌐 **Location:** Villa Mercedes, San Luis, Argentina
- 🚀 **Open to:** Remote/Hybrid COBOL/Mainframe Developer positions (Trainee/Junior)

### Languages
- 🇪🇸 Spanish (Native)
- 🇬🇧 English (B2 Upper Intermediate - EF SET 2024)

---

## 📚 Academic Documentation

This project was developed as an Associate Degree capstone project and includes comprehensive academic documentation:

- **Thesis Document:** [Complete PDF (56 pages)](./thesis_document/TESIS.pdf)
  - UML diagrams (use cases, class diagrams, sequence diagrams)
  - Detailed architecture analysis
  - Implementation decisions and trade-offs
  - Operation logs and testing evidence
  - Security incident analysis
  - Performance metrics

- **Defense:** December 16, 2025 - Grade: 9/10
- **Context:** Only project from 2024-2025 cohort fully implemented vs theoretical proposals

---

## 📄 License

This project was developed for academic purposes. Code is available for review and learning purposes.

---

## 🙏 Acknowledgments

- **IBM LinuxONE Community Cloud** for providing free access to mainframe infrastructure
- **Universidad Nacional de Villa Mercedes** for academic guidance
- **IBM Skills Network** for mainframe training and certification resources
- **Interskill Learning** for Zowe and technical skills training

---

## 🌐 Topics

`cobol` `mainframe` `ibm-linuxone` `s390x` `python` `flask` `db2` `hybrid-architecture` `batch-processing` `enterprise-systems` `gnucobol` `docker` `inventory-management` `thesis-project` `academic-project` `fefo` `barcode-scanner` `production-deployment`

---

---

# 🇪🇸 Versión en Español

# Sistema de Gestión Comercial: Arquitectura Híbrida Kiosco en IBM LinuxONE

[![Status](https://img.shields.io/badge/Estado-Completado-success)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Arquitectura](https://img.shields.io/badge/Arquitectura-H%C3%ADbrida%20(Web%20%2B%20Mainframe)-blue)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Plataforma](https://img.shields.io/badge/Plataforma-IBM%20LinuxONE%20(s390x)-purple)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Stack](https://img.shields.io/badge/Stack-Python%20|%20Flask%20|%20COBOL%20|%20Db2%20|%20Docker-blueviolet)](https://github.com/Flor125/kiosco-management-system-mainframe)
[![Nota](https://img.shields.io/badge/Nota-9%2F10-brightgreen)](https://github.com/Flor125/kiosco-management-system-mainframe)

> **🏆 Contexto Académico:** Único proyecto de la cohorte 2024-2025 completamente implementado, desplegado y funcionando (vs propuestas teóricas del resto de estudiantes). Desarrollado durante 2024-2025, defendido el 16 de diciembre de 2025 con nota 9/10 en la Universidad Nacional de Villa Mercedes.
> 
> 📄 **Tesis Académica Completa:** [Ver PDF (56 páginas)](./thesis_document/tesis.pdf) - Incluye diagramas UML, casos de uso, arquitectura detallada y logs de operación.

---

## 📋 Descripción General

Sistema de gestión de inventario y ventas para kioscos minoristas desplegado en **IBM LinuxONE (arquitectura mainframe s390x)**. Combina la agilidad de una interfaz web moderna con la robustez del procesamiento batch legacy, demostrando integración real de sistemas mainframe empresariales con tecnologías contemporáneas.

**Logro clave:** Despliegue en producción real en infraestructura empresarial IBM manejando tráfico de internet real e incidentes de seguridad, no una simulación de escritorio.

---

## 🚀 Arquitectura Híbrida

El sistema gestiona inventario utilizando dos paradigmas:

1. **Online (Tiempo real):** Python + Flask manejan la interfaz móvil, escaneo de códigos de barras y transacciones directas a la base de datos **IBM Db2** containerizada.
2. **Batch (Back-office):** Un módulo **COBOL** puro procesa grandes volúmenes de datos para cálculo de costos promedio, detección de vencimientos y generación de históricos.

### 🔄 Flujo de Integración (Enfoque "Zero SQL")

Debido a restricciones de compilación en el entorno mainframe s390x (GnuCOBOL sin precompilador Enterprise), se implementó una integración basada en intercambio de archivos planos, simulando procesamiento batch mainframe clásico:

1. **Extracción:** Python extrae datos de Db2 y genera `costos.dat` y `vencimientos.dat` (archivos secuenciales).
2. **Procesamiento:** El programa COBOL (`batchcosto.cbl`) lee los archivos secuenciales, aplica lógica de negocio (reglas de ganancia, alertas FEFO) y genera `historico.dat` y `alertas.dat`.
3. **Carga:** Python reinyecta los resultados procesados en Db2 para visualización en el frontend.

**¿Por qué este enfoque?** GnuCOBOL en s390x carece de soporte nativo EXEC SQL, requiriendo un workaround que en realidad refleja patrones reales de integración mainframe-moderno usados en banca y seguros.

---

## ✨ Características Principales

* **Escaneo Móvil:** Consulta de precios y stock en tiempo real usando cámara del celular (lector de códigos de barras QuaggaJS).
* **Lógica FEFO (First Expired, First Out):** El sistema descuenta automáticamente el lote con vencimiento más próximo al procesar ventas.
* **Alertas Inteligentes:** Detección automática de productos próximos a vencer con sugerencias de "Combos Oferta" para liquidar inventario.
* **Procesamiento Batch:** Trabajos nocturnos en COBOL calculan costos promedio ponderados y generan reportes de inteligencia de negocio.
* **Infraestructura Enterprise:** Desplegado en **IBM LinuxONE Community Cloud** con Db2 en contenedores Docker.
* **Seguridad Production-Ready:** Gestión de sesiones, hashing de contraseñas, logs de auditoría, aislamiento Docker.

---

## 🛠 Stack Tecnológico

* **Frontend:** HTML5, CSS3, JavaScript (QuaggaJS para escaneo de códigos de barras)
* **Backend API:** Python 3.10, Flask, driver ibm_db
* **Procesamiento Batch:** COBOL (GnuCOBOL para s390x)
* **Base de Datos:** IBM Db2 Community Edition (Dockerizada en s390x)
* **Plataforma:** IBM LinuxONE Community Cloud (arquitectura mainframe s390x)
* **OS:** Ubuntu Server 20.04 para s390x
* **Seguridad:** Hashing pbkdf2:sha256, validación de sesiones, protección CSRF, RBAC

---

## 📸 Capturas de Pantalla

Diseño *mobile-first* optimizado para operarios de kiosco.

| Acceso y Seguridad | Panel Principal |
|:---:|:---:|
| <img src="./screenshots_thesis/login.png" width="300"> | <img src="./screenshots_thesis/system1.png" width="300"> |
| **Login Seguro & Registro** | **Dashboard & Alertas** |

| Gestión de Stock (FEFO) | Registro de Compras |
|:---:|:---:|
| <img src="./screenshots_thesis/system4.png" width="300"> | <img src="./screenshots_thesis/system2.png" width="300"> |
| **Visualización por Lotes y Vencimientos** | **Ingreso de Mercadería & Escáner** |

---

## 🏗️ Arquitectura de Despliegue

**IMPORTANTE:** Este proyecto fue desplegado en **IBM LinuxONE Community Cloud** (arquitectura mainframe s390x), no en servidores x86_64 estándar.

### Requisitos de Infraestructura
- **Plataforma:** IBM LinuxONE Community Cloud (arquitectura mainframe s390x)
- **OS:** Ubuntu Server 20.04 para s390x
- **Base de Datos:** IBM Db2 Community Edition (contenedor Docker para s390x)
- **Compilador COBOL:** GnuCOBOL compilado para s390x (soporte EXEC SQL limitado)
- **Python:** 3.10+ con driver ibm_db para s390x
- **Red:** Exposición IP pública para pruebas de producción

### Por Qué Este Despliegue es Único

1. **Entorno mainframe real:** No una simulación o emulación x86_64 - infraestructura empresarial IBM real
2. **Restricciones de arquitectura:** Binarios específicos s390x, herramientas limitadas comparadas con entornos de desarrollo x86_64
3. **Configuración production-grade:** Expuesto a internet, manejando tráfico real y amenazas de seguridad
4. **Desafíos de integración:** GnuCOBOL en s390x carece de precompilador EXEC SQL, requiriendo patrón de integración por archivos planos
5. **Base de datos empresarial:** IBM Db2 corriendo en entorno mainframe, no SQLite o MySQL

### Estructura del Proyecto (Híbrida)
```
kiosco-management-system-mainframe/
├── app.py                  # Aplicación web Flask (Python)
├── cobol/
│   ├── batchcosto.cbl     # Lógica de procesamiento batch (COBOL)
│   ├── costos.dat         # Entrada: Datos de compras para batch
│   ├── historico.dat      # Salida: Promedios calculados
│   └── alertas.dat        # Salida: Alertas de vencimiento
├── data/                   # Scripts de inicialización de base de datos
├── templates/              # Templates HTML (mobile-first)
├── static/                 # CSS, JavaScript, assets
├── screenshots_thesis/     # Evidencia de despliegue
└── thesis_document/        # Tesis académica completa (56 páginas)
```

Esta estructura demuestra la **integración de dos mundos**: la carpeta `cobol/` contiene lógica batch y archivos de intercambio, conviviendo con la aplicación Flask.

![Estructura VS Code](./screenshots_thesis/vscode_structure.png)

### Notas de Replicación

Este proyecto **no puede replicarse fácilmente** en máquinas de desarrollo estándar (x86_64). Requiere:
- Acceso a IBM LinuxONE Community Cloud (tier gratuito disponible para desarrolladores en [linuxone.cloud.marist.edu](https://linuxone.cloud.marist.edu))
- Comprensión de compilación y despliegue específicos para s390x
- Configuración de Db2 para arquitectura mainframe
- Experiencia con patrones de procesamiento batch mainframe

**Para reclutadores/revisores:** Los logs de operación completos, screenshots y tesis académica (56 páginas) proveen evidencia comprensiva de despliegue y funcionalidad.

---

## 🛡️ Despliegue en Producción y Seguridad

### Operación en Mundo Real

**El sistema fue desplegado en IBM LinuxONE (s390x) y expuesto a internet** para pruebas, demostración y evaluación académica.

**Incidentes de seguridad documentados durante operación:**
- ✅ Múltiples ataques de bots automatizados detectados y bloqueados
- ✅ Intentos de acceso no autorizado registrados y rechazados
- ✅ Intentos de secuestro de sesión prevenidos
- ✅ Patrones de inyección SQL filtrados
- ✅ Intentos de fuerza bruta en login limitados por tasa

**Implementación de seguridad:**
- Hashing de contraseñas: pbkdf2:sha256 con salt
- Validación de sesiones y gestión de timeout
- Protección CSRF en todos los endpoints POST
- Control de acceso basado en roles (JEFE/EMPLEADO)
- Aislamiento de contenedores Docker en plataforma mainframe
- Rastro de auditoría completo de todas las operaciones
- Validación de entrada y consultas parametrizadas

![Logs del Servidor](./screenshots_thesis/logs.png)

*Evidencia: Logs de producción mostrando tráfico real, incidentes de seguridad y estabilidad del sistema en infraestructura mainframe*

---

## 🎯 Por Qué Este Proyecto Importa

### Problema de Negocio

Pequeños comercios minoristas (kioscos) en Argentina enfrentan:
- Control de inventario manual → Errores humanos en conteo de stock
- Falta de seguimiento de vencimientos → Desperdicio de productos y pérdidas
- Sin trazabilidad a nivel de lote → Problemas de cumplimiento y auditoría
- Cálculos de precios complejos → Oportunidades de ingresos perdidas
- Rotación de stock ineficiente → Capital inmovilizado en inventario viejo

### Desafío Técnico

Construir un sistema de grado empresarial en **infraestructura mainframe** (IBM LinuxONE s390x) que:
- Maneje transacciones web en tiempo real con UI responsive
- Procese trabajos batch nocturnos (COBOL) para computaciones pesadas
- Integre tecnologías legacy y modernas sin fisuras
- Opere bajo restricciones de compilación (GnuCOBOL sin EXEC SQL)
- Mantenga transacciones ACID y consistencia de datos
- Corra en producción con amenazas de seguridad reales

### Impacto de la Solución

**Para operadores:**
- Interfaz mobile-first accesible desde cualquier smartphone
- Escaneo de códigos de barras para búsqueda instantánea de productos
- Visibilidad de stock en tiempo real por lote y fecha de vencimiento
- Alertas automatizadas previenen venta de productos vencidos

**Para el negocio:**
- Lógica FEFO minimiza desperdicio (First Expired, First Out)
- Precios inteligentes con cálculo de costo promedio ponderado
- Análisis y optimización de rotación de inventario
- Sugerencias de combos/ofertas basadas en patrones de vencimiento

**Para desarrolladores:**
- Demuestra patrones de arquitectura híbrida (web + batch)
- Muestra despliegue mainframe real (no simulación)
- Prueba viabilidad de integración legacy-moderno
- Provee ejemplo de implementación de seguridad en producción

### Logro Técnico

**Más Allá del Desarrollo Web Estándar:**
- ✅ Desplegado en **infraestructura mainframe IBM real** (no simulación x86_64)
- ✅ Manejó **restricciones de arquitectura s390x** (herramientas limitadas, binarios diferentes)
- ✅ Resolvió **limitaciones de GnuCOBOL** en s390x (sin EXEC SQL → integración por archivos planos)
- ✅ Gestionó **base de datos de grado empresarial** (Db2) en entorno mainframe
- ✅ Operó en **producción** con tráfico de internet real e incidentes de seguridad

Esto demuestra experiencia con:
- Sistemas mainframe empresariales verdaderos (no solo COBOL en escritorio)
- Patrones de integración legacy-moderno usados en banca/seguros
- Resolución de problemas con recursos restringidos
- Despliegue y operaciones en producción
- Manejo de incidentes de seguridad y logging

---

## 👤 Autora

**Florencia Alicia Sombra**

### Educación
🎓 Tecnicatura en Programación de Sistemas  
Universidad Nacional de Villa Mercedes (2022-2026)

### Certificaciones
- 📜 **IBM Mainframe Developer Professional Certificate** (2024-2025)
- 📜 **IBM z/OS Mainframe Practitioner Professional Certificate** (2024-2025)
- 📜 **IBM Z Xplore** - Concepts, Advanced, All Star (2024)
- 📜 **Fundamentals for Zowe** - Interskill Learning (2025)
- 📜 **Coaching and Mentoring for Technical Specialists** - Interskill Learning (2025)

### Habilidades Técnicas
**Mainframe:** COBOL, JCL, REXX, z/OS, TSO, VSAM, Db2, Zowe, z/OSMF, Ansible  
**Backend:** Python, Flask, REST APIs, Procesamiento Batch  
**Infraestructura:** Docker, Linux, IBM LinuxONE (s390x), Git/GitHub

### Contacto y Enlaces
- 💼 **LinkedIn:** [linkedin.com/in/flor125](https://linkedin.com/in/flor125)
- 💻 **GitHub:** [github.com/Flor125](https://github.com/Flor125)
- 📧 **Email:** sombraflorencia097@gmail.com
- 🌐 **Ubicación:** Villa Mercedes, San Luis, Argentina
- 🚀 **Disponible para:** Posiciones Remote/Híbridas como Desarrolladora COBOL/Mainframe (Trainee/Junior)

### Idiomas
- 🇪🇸 Español (Nativo)
- 🇬🇧 Inglés (B2 Upper Intermediate - EF SET 2024)

---

## 📚 Documentación Académica

Este proyecto fue desarrollado como tesis de tecnicatura e incluye documentación académica comprensiva:

- **Documento de Tesis:** [PDF Completo (56 páginas)](./thesis_document/tesis.pdf)
  - Diagramas UML (casos de uso, diagramas de clase, diagramas de secuencia)
  - Análisis de arquitectura detallado
  - Decisiones de implementación y trade-offs
  - Logs de operación y evidencia de testing
  - Análisis de incidentes de seguridad
  - Métricas de rendimiento

- **Defensa:** 16 de diciembre de 2025 - Nota: 9/10
- **Contexto:** Único proyecto de la cohorte 2024-2025 completamente implementado vs propuestas teóricas

---

## 📄 Licencia

Este proyecto fue desarrollado con fines académicos. El código está disponible para revisión y propósitos de aprendizaje.

---

## 🙏 Agradecimientos

- **IBM LinuxONE Community Cloud** por proveer acceso gratuito a infraestructura mainframe
- **Universidad Nacional de Villa Mercedes** por guía académica
- **IBM Skills Network** por recursos de entrenamiento y certificación mainframe
- **Interskill Learning** por entrenamiento en Zowe y habilidades técnicas

---

## 🌐 Temas

`cobol` `mainframe` `ibm-linuxone` `s390x` `python` `flask` `db2` `arquitectura-hibrida` `procesamiento-batch` `sistemas-empresariales` `gnucobol` `docker` `gestion-inventario` `proyecto-tesis` `proyecto-academico` `fefo` `escaner-codigo-barras` `despliegue-produccion`
