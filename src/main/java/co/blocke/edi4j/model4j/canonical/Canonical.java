// Canonical.java
package co.blocke.edi4j.model4j.canonical;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Canonical {
    private static final Pattern EXTRACT_KEY = Pattern.compile(".*/([^/]+)$");

    /** Extract the final segment of a “#/…/key” ref, or fail. */
    public static String extractRefKey(String ref) throws CanonicalError {
        Matcher m = EXTRACT_KEY.matcher(ref);
        if (m.matches()) {
            return m.group(1);
        }
        throw new CanonicalError("Unable to extract a reference key from " + ref);
    }
}

