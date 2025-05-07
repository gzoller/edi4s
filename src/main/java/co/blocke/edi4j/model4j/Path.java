package co.blocke.edi4j.model4j;

import java.util.Objects;

/**
 * Tracks a flattened path through a nested structure, joined by dots or >.
 */
public class Path {
    private final String value;

    /**
     * Creates an empty path.
     */
    public Path() {
        this("");
    }

    /**
     * Creates a path with the given string value.
     * @param value the path string (e.g. "root.child.grandchild")
     */
    public Path(String value) {
        this.value = Objects.requireNonNull(value);
    }

    /**
     * Returns the prefix of the path (everything before the last dot).
     * If there is no dot, returns this Path unchanged.
     */
    public Path prefix() {
        int idx = value.lastIndexOf('.');
        if (idx == -1) {
            return this;
        }
        return new Path(value.substring(0, idx));
    }

    @Override
    public String toString() {
        return value;
    }

    /**
     * Appends a terminal segment to the path (with a dot separator),
     * or returns the terminal if the current path is empty.
     */
    public String toStringWith(String terminal) {
        if (value.isEmpty()) {
            return terminal;
        }
        return value + "." + terminal;
    }

    /**
     * Returns a new Path by dot‑joining the given level.
     */
    public Path dot(String level) {
        if (value.isEmpty()) {
            return new Path(level);
        }
        return new Path(value + "." + level);
    }

    /**
     * Returns a new Path by nesting the given level using '>' delimiter.
     */
    public Path nest(String level) {
        if (value.isEmpty()) {
            return new Path(level);
        }
        return new Path(value + ">" + level);
    }
}