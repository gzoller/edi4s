// EdiEnum.java
package co.blocke.edi4j.model4j.canonical;

import java.util.List;

/** A closed‐set (“enum”) type in the schema. */
public class EdiEnum implements EnumOrSchema {
    private final List<String> enums;
    private final String type;
    private final String format;

    public EdiEnum(List<String> enums, String type, String format) {
        this.enums = enums;
        this.type = type;
        this.format = format;
    }

    public List<String> getEnums() {
        return enums;
    }

    public String getType() {
        return type;
    }

    public String getFormat() {
        return format;
    }
}

