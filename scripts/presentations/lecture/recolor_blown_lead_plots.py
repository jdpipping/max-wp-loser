from pathlib import Path

from PIL import Image


PENN_BLUE = (1, 31, 91)
ACCENT_RED = (170, 45, 45)

# Original research-note palette used in the copied lecture assets.
SOURCE_RED = (255, 107, 107)
SOURCE_BLUE = (0, 0, 255)


def recolor_image(path: Path) -> None:
    image = Image.open(path).convert("RGBA")
    pixels = image.load()
    width, height = image.size

    for y in range(height):
        for x in range(width):
            r, g, b, a = pixels[x, y]
            if a == 0:
                continue

            # Preserve anti-aliasing by matching within a small RGB radius.
            if abs(r - SOURCE_RED[0]) <= 20 and abs(g - SOURCE_RED[1]) <= 20 and abs(b - SOURCE_RED[2]) <= 20:
                pixels[x, y] = (*ACCENT_RED, a)
            elif abs(r - SOURCE_BLUE[0]) <= 20 and g <= 40 and abs(b - SOURCE_BLUE[2]) <= 20:
                pixels[x, y] = (*PENN_BLUE, a)

    image.save(path)


def main() -> None:
    repo_root = Path(__file__).resolve().parents[3]
    plot_dir = repo_root / "presentations" / "lecture" / "plots"
    for name in [
        "high-wp-game.png",
        "blown_sim_distribution.png",
        "blown_sim_threshold.png",
    ]:
        recolor_image(plot_dir / name)


if __name__ == "__main__":
    main()
