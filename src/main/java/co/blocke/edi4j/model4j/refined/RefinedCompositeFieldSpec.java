// RefinedCompositeFieldSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;
import java.util.Optional;

public class RefinedCompositeFieldSpec implements RefinedFieldSpec {
    private final String name;
    private final String canonicalName;
    private final Optional<String> humanName;
    private final int index;
    private final String description;
    private final boolean required;
    private final List<RefinedSingleFieldSpec> components;

    public RefinedCompositeFieldSpec(
        String name,
        String canonicalName,
        Optional<String> humanName,
        int index,
        String description,
        boolean required,
        List<RefinedSingleFieldSpec> components
    ) {
        this.name           = name;
        this.canonicalName  = canonicalName;
        this.humanName      = humanName;
        this.index          = index;
        this.description    = description;
        this.required       = required;
        this.components     = components;
    }

    @Override public String getName()             { return name; }
    @Override public String getCanonicalName()    { return canonicalName; }
    @Override public Optional<String> getHumanName() { return humanName; }
    @Override public int getIndex()               { return index; }
    @Override public String getDescription()      { return description; }
    @Override public boolean isRequired()         { return required; }

    public List<RefinedSingleFieldSpec> getComponents() {
        return components;
    }
}

