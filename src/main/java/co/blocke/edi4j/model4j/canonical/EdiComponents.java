// EdiComponents.java
package co.blocke.edi4j.model4j.canonical;

import java.util.Map;

public class EdiComponents {
    private final Map<String, EnumOrSchema> schemas;

    public EdiComponents(Map<String, EnumOrSchema> schemas) {
        this.schemas = schemas;
    }

    public Map<String, EnumOrSchema> getSchemas() {
        return schemas;
    }
}

