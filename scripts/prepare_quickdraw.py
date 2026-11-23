
"""Prepare a small reproducible sample of Quick, Draw! 28x28 grayscale images.

This downloads an initial subset per category and splits it into training
and held-out examples; it is not a random sample of the entire dataset.
"""

import ast
import random
import urllib.parse
import urllib.request
from pathlib import Path

CATEGORIES = ["cat", "tree", "house", "bicycle", "airplane"]

TRAIN_PER_CATEGORY = 100
TEST_PER_CATEGORY = 25

N = TRAIN_PER_CATEGORY + TEST_PER_CATEGORY
PIXELS = 28 * 28

OUTPUT = Path("data/quickdraw_demo.tsv")


def get_bytes(url, start, end):
    """Return bytes [start, end] inclusive, requesting an HTTP byte range.

    Raise RuntimeError if the server does not return an exact partial response.
    Network failures are propagated to the caller.
    """
    request = urllib.request.Request(
        url,
        headers={"Range": f"bytes={start}-{end}"}
    )

    with urllib.request.urlopen(request, timeout=60) as response:
        if response.status != 206:
            raise RuntimeError("Server did not support partial download")
        data = response.read()

    if len(data) != end - start + 1:
        raise RuntimeError("Incomplete download")

    return data


def load_category(name):
    """Return the first N 784-byte images of Google's named .npy category.

    Validate the NumPy header and ensure the data represent 28x28 uint8
    images. Raise ValueError for an unexpected format; network and range
    errors propagate from get_bytes.
    """
    url = (
        "https://storage.googleapis.com/"
        "quickdraw_dataset/full/numpy_bitmap/"
        + urllib.parse.quote(name) + ".npy"
    )

    header_bytes = get_bytes(url, 0, 4095)

    if header_bytes[:6] != b"\x93NUMPY":
        raise ValueError("Not a NumPy dataset")

    if header_bytes[6] == 1:
        header_start = 10
        size = int.from_bytes(header_bytes[8:10], "little")
    else:
        header_start = 12
        size = int.from_bytes(header_bytes[8:12], "little")

    header_end = header_start + size

    if header_end > len(header_bytes):
        raise ValueError("Unexpectedly long NumPy header")

    header = ast.literal_eval(
        header_bytes[header_start:header_end].decode("latin1")
    )

    if (
        header["descr"] not in ("|u1", "<u1")
        or header["fortran_order"]
        or header["shape"][0] < N
        or header["shape"][1:] not in ((784,), (28, 28))
    ):
        raise ValueError("Unexpected image format")

    raw = get_bytes(
        url,
        header_end,
        header_end + N * PIXELS - 1
    )

    return [
        raw[i * PIXELS:(i + 1) * PIXELS]
        for i in range(N)
    ]


def main():
    """Write the selected training/test examples to OUTPUT in TSV format.

    Every row contains split, category, and 784 comma-separated byte values.
    The fixed shuffle seed makes repeated runs reproducible.
    """
    OUTPUT.parent.mkdir(exist_ok=True)

    with OUTPUT.open("w", encoding="utf-8") as file:
        for category in CATEGORIES:
            print("Downloading:", category)

            images = load_category(category)

            # Shuffle the downloaded subset before making a reproducible split.
            random.Random(3110).shuffle(images)

            for i, image in enumerate(images):
                split = (
                    "train" if i < TRAIN_PER_CATEGORY else "test"
                )

                numbers = ",".join(str(pixel) for pixel in image)

                file.write(
                    f"{split}\t{category}\t{numbers}\n"
                )

    print("Saved", OUTPUT)
    print("500 reference drawings and 125 separate test drawings")


if __name__ == "__main__":
    main()
