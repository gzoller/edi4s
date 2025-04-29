// EdiItemsProperty.java
package co.blocke.edi4j.model4j.canonical;

import java.util.Map;
import java.util.Optional;

public class EdiItemsProperty implements Property, RefOrItemsProperty {
    private final String type;
    private final Optional<Integer> minItems;
    private final Optional<Integer> maxItems;
    private final EdiRefProperty items;

    public EdiItemsProperty(
        String type,
        Optional<Integer> minItems,
        Optional<Integer> maxItems,
        EdiRefProperty items
    ) {
        this.type     = type;
        this.minItems = minItems;
        this.maxItems = maxItems;
        this.items    = items;
    }

    @Override
    public EnumOrSchema dereference(Map<String, EnumOrSchema> schemas) throws CanonicalError {
        return items.dereference(schemas);
    }

    /** True if the first property is a reference (i.e. loop body). */
    public boolean loopHasBody(Map<String, EnumOrSchema> schemas) throws CanonicalError {
        EnumOrSchema deref = dereference(schemas);
        if (deref instanceof EdiSchema) {
            EdiSchema schema = (EdiSchema) deref;
            return schema.getProperties()
                         .entrySet()
                         .stream()
                         .findFirst()
                         .map(e -> e.getValue() instanceof EdiRefProperty)
                         .orElse(false);
        }
        return false;
    }

    public String getType() {
        return type;
    }

    public Optional<Integer> getMinItems() {
        return minItems;
    }

    public Optional<Integer> getMaxItems() {
        return maxItems;
    }

    public EdiRefProperty getItems() {
        return items;
    }
}

