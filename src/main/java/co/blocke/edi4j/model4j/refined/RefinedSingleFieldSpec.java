// RefinedSingleFieldSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;
import java.util.Optional;

public class RefinedSingleFieldSpec implements RefinedFieldSpec {
    private final String name;
    private final String canonicalName;
    private final Optional<String> humanName;
    private final int index;
    private final String description;
    private final boolean required;
    private final String dataType;
    private final Optional<String> format;
    private final Optional<Integer> elementId;
    private final List<String> validValues;
    private final Optional<String> validValuesRef;

    public RefinedSingleFieldSpec(
        String name,
        String canonicalName,
        Optional<String> humanName,
        int index,
        String description,
        boolean required,
        String dataType,
        Optional<String> format,
        Optional<Integer> elementId,
        List<String> validValues,
        Optional<String> validValuesRef
    ) {
        this.name            = name;
        this.canonicalName   = canonicalName;
        this.humanName       = humanName;
        this.index           = index;
        this.description     = description;
        this.required        = required;
        this.dataType        = dataType;
        this.format          = format;
        this.elementId       = elementId;
        this.validValues     = validValues;
        this.validValuesRef  = validValuesRef;
    }

    @Override public String getName()             { return name; }
    @Override public String getCanonicalName()    { return canonicalName; }
    @Override public Optional<String> getHumanName() { return humanName; }
    @Override public int getIndex()               { return index; }
    @Override public String getDescription()      { return description; }
    @Override public boolean isRequired()         { return required; }

    public String getDataType()                   { return dataType; }
    public Optional<String> getFormat()           { return format; }
    public Optional<Integer> getElementId()       { return elementId; }
    public List<String> getValidValues()          { return validValues; }
    public Optional<String> getValidValuesRef()   { return validValuesRef; }
}

