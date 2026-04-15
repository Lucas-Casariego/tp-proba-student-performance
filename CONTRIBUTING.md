# Flujo de Trabajo

## 1. Crear una rama nueva

Nunca trabajes directamente sobre `main`. Mantén el código estable separado de las nuevas funcionalidades.

```bash
git checkout -b feature/nombre-descriptivo
```

Ejemplos:

- `feature/login`
- `bugfix/error-validacion`

## 2. Hacer cambios y commitear

Realiza tus modificaciones y guarda el progreso localmente con mensajes claros.

```bash
git add .
git commit -m "Descripción clara del cambio"
```

## 3. Subir la rama

Envía tus cambios al repositorio remoto.

```bash
git push origin feature/nombre-descriptivo
```

## 4. Crear Pull Request (PR)

1. Ve al repositorio de Github
2. Crea un Pull Request apuntando hacia la rama `main`.
3. Agrega una descripción

## 🔒 Reglas Importantes

- ❗ **No se puede pushear directo a `main`**: Todos los cambios deben pasar obligatoriamente por un PR.
- 👀 **Revisión obligatoria**:
  - Se requieren al menos 2 aprobaciones.
- 🚫 **No force push**: Está prohibido reescribir la historia en ramas compartidas.

## ✅ Buenas Prácticas

- Hacer PRs pequeños y con un solo objetivo.
- Usar nombres descriptivos para las ramas.
- Escribir commits entendibles (atómicos y claros).
- Responder comentarios en los PRs de forma constructiva.
- No mezclar múltiples features en un mismo PR.

## 🧠 Ejemplo de Flujo Completo

```bash
# 1. Crear rama de trabajo
git checkout -b feature/login

# 2. Realizar cambios en el código
# [Edición de archivos]

# 3. Registrar los cambios
git add .
git commit -m "Agrega endpoint de login"

# 4. Subir al servidor
git push origin feature/login
```

Pasos finales:

1. Crear el PR en la interfaz web.
2. Esperar la revisión y aprobación de los pares.
3. Realizar el Merge.
