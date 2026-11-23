# QuickSketch — k-NN Classifier

**Owner:** Oded Harazi  
**Next step:** Madhav Girish — recognition improvements and evaluation

## Overview

This module classifies sketches using **k-nearest neighbors (k-NN)**. It compares an input feature vector against labeled reference drawings, finds the `k` closest examples, and predicts the category receiving the most votes.

The classifier works with numerical vectors of any length, provided every vector in a classification run has the same length. For QuickSketch, the intended format is **28×28 grayscale images**, represented by 784 floats between 0 and 1.

## Implementation

- `lib/knn.mli` — public types and function signatures.
- `lib/knn.ml` — squared Euclidean distance, nearest-neighbor selection, majority voting, ranked predictions, and input validation.
- `test/test_knn.ml` — 15 OUnit tests covering classification, distances, ties, invalid inputs, and edge cases.
- `bin/demo_knn.ml` — small demonstration using artificial vectors. This is a quick sanity check which doesn't depend on downloaded data. 
- `bin/demo_real_knn.ml` — evaluates predictions on real Google Quick, Draw! images. Larger demo, and measures accuracy better. 
- `scripts/prepare_quickdraw.py` — prepares the real image dataset for the OCaml demo.

Using the classifier: Call Knn.predict ~k:3 ~training query, where training is a list of labeled reference drawings and query is the new drawing to classify. The function compares the query against the reference drawings and returns ranked guesses, or an error if the input is invalid.

Drawings must first be converted to numerical vectors. For our project, Max handles that conversion into 784 grayscale values. My classifier handles the recognition.

## Running

From the project root, after setting up the OCaml environment:

```bash
dune build
dune test --force
dune exec bin/demo_knn.exe
```

For the real-image evaluation:

```bash
python3 scripts/prepare_quickdraw.py
dune exec bin/demo_real_knn.exe
```

Note that the python script:
Downloads 125 real Google Quick, Draw! images from each of five categories (625 images total) onto your computer.
Splits them into 500 training drawings and 125 testing drawings.
Saves them locally in data/quickdraw_demo.tsv.

The Python preparation step is needed only when the sample data file is missing or needs regeneration (first time).

## Current baseline

Using five Quick, Draw! categories (cat, tree, house, bicycle, airplane), with **100 reference images and 25 held-out test images per category**, the `k=3` classifier achieved:

- **Overall:** 106/125 correct (84.8%).
- **Cat:** 56%; **Tree:** 88%; **House:** 92%; **Bicycle:** 88%; **Airplane:** 100%.

This is a preliminary evaluation on a small subset of Google's dataset, not an estimate of accuracy for the completed game.

## For Madhav

The baseline is ready for accuracy experiments. Useful next steps are comparing `k=1`, `k=3`, `k=5`, and other values; testing larger and more representative datasets; and investigating weighted voting or alternative distance metrics. Also may be worth noting that this implementation sorts through all reference drawings for each prediction, whereas an optimization may optimize a larger dataset.

Use the existing test split to compare approaches fairly. Keep baseline behavior available for comparison, and add tests for any changes to prediction behavior.

**Integration:** Once image preprocessing is available, pass its output as a `float array` to `Knn.predict`. The input dimensions and pixel normalization must match the training vectors.

## Integration details

To use the classifier from another OCaml module:

```ocaml
open Quicksketch

let result = Knn.predict ~k:3 ~training query
```

- `training` must be a `Knn.sample list` containing labeled reference drawings.
- `query` must be a `float array` containing the drawing to recognize.
- For our Quick, Draw! dataset, vectors contain 784 values (28×28), with the background near 0.0 and drawn strokes near 1.0.
- `Knn.predict` does not automatically load training data. The current dataset loader is in `bin/demo_real_knn.ml`; the full game will need a reusable loader.
- `Knn.squared_distance` returns squared Euclidean distance, while each prediction's `nearest_distance` is the ordinary Euclidean distance.

**Dataset attribution:** Google Creative Lab, [Quick, Draw! Dataset](https://github.com/googlecreativelab/quickdraw-dataset), CC BY 4.0.


Note by Oded: I used ChatGPT to help design, generate, debug, and document the k-NN classifier, OUnit tests, and demonstration programs. I reviewed the generated code, resolved compilation issues, and verified its behavior through unit tests and evaluation on the Google Quick, Draw! dataset.