// EdiRefProperty.java
package co.blocke.edi4j.model4j.canonical;

import java.util.Map;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EdiRefProperty implements Property, RefOrItemsProperty {
    private final String ref;

    public EdiRefProperty(@JsonProperty("$ref") String ref) {
        this.ref = ref;
    }

    @Override
    public EnumOrSchema dereference(Map<String, EnumOrSchema> schemas) throws CanonicalError {
        String key = Canonical.extractRefKey(ref);
        EnumOrSchema v = schemas.get(key);
        if (v != null) {
            return v;
        }
        throw new CanonicalError("Reference for " + ref + " not found in canonical schema");
    }
}

