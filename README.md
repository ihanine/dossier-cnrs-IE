# Dossier CNRS IE — site de consultation

Site statique pour le jury : aucune dépendance, aucun JavaScript.
Une seule feuille de style (`style.css`) gouverne tout l'aspect.

## Arborescence

```
index.html            accueil (en-tête, photo, lien CV, titre du concours, expériences)
lisez-moi.html        notice d'organisation
style.css             style unique (modifier ici = modifier tout le site)
photo.jpg             ta photo (à déposer ; affichée en haut à gauche)
cv.pdf                ton CV (à déposer ; cible du lien « Curriculum vitae »)
supelec/ sfen/ rte/ cnrs-ie/ ehess/
    index.html        page d'expérience : liste des projets
    <projet>/
        index.html    page de projet : documents (PDF, code, support)
        *.pdf *.R ...  les fichiers eux-mêmes
```

`ehess/` et `ehess/n2o-carbone/` sont remplis en exemple. Les autres pages
sont des gabarits avec des [crochets] à compléter.

## Ajouter un document

Déposer le fichier dans le dossier du projet, puis ajouter dans son `index.html` :

```html
<li>
  <span class="tag">PDF</span><a class="doc" href="rapport.pdf">Titre</a>
  <p class="caption">Une phrase décrivant le document.</p>
</li>
```

Tags : `PDF`, `R`, `PPTX`, `ZIP`, `HTML`.

## Publier une modification

```
git add .
git commit -m "ce que j'ai changé"
git push
```

Puis recharger le site avec Ctrl-Maj-R (cache du navigateur).
