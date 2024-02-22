# Halima KSAL

# Projet Final : Cours Interpréteur et Compilateur

Utilisation:


*  ocamlbuild -use-menhir main.byte 
* ./main.byte tests/bool.test
* ./main.byte tests/string.test
* ./main.byte tests/boucle.test
* ./main.byte tests/condition.test


* ./main.byte tests/boucle.test > boucle.s 

# Projet : Compilateur simple en Ocaml

1. Analyse Lexicale et Syntaxique :

Utilisation de lexer.mll pour l'analyse lexicale, pour définir les tokens du langage.

Utilisation de parser.mly pour l'analyse syntaxique, pour définir la grammaire du langage.

2. Génération de Code MIPS :

Le fichier mips.ml gère la représentation et la génération du code MIPS, essentiel pour la phase de compilation.

3. Structure du Langage Source :

Les fichiers ast.ml et semantics.ml définissent la structure abstraite du langage et sa sémantique.

simplifier.ml  : pour l'optimisation du code intermédiaire.

4. Infrastructure de Compilation :

compiler.ml  : joue un rôle central dans la coordination des différentes étapes de compilation.

main.ml : sert de point d'entrée pour le compilateur.

5. Bibliothèque de Base :

baselib.ml fournit des fonctions de base utilisées par le compilateur.


# Le projet couvre une grande partie des caractéristiques attendues du langage que je devais implémenter tels que :

- Types de Base :

* Entiers, Booléens, Chaînes de Caractères

- Bibliothèque de Base :

* Lecture et Écriture sur l'Entrée et la Sortie Standard

* Opérateurs Logiques de Base sur les booléens

* Opérateurs Arithmétiques de Base sur les Entiers : Les opérations telles que l'addition, la soustraction, la multiplication, la division et le modulo sont présentes

- Expression :

* Valeur, Variable, Appel de Fonction

- Instructions :

* Déclaration de Variable, Assignation, Retour de Valeur

* Branchement Conditionnel et Boucles

- Structure de Programme :

* Programme et Fonctions (y compris main)

- Blocs et Corps de Fonction 

