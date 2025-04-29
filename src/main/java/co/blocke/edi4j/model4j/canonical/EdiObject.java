// EdiObject.java
package co.blocke.edi4j.model4j.canonical;

public class EdiObject {
    private final String openapi;
    private final EdiInfo info;
    private final EdiComponents components;

    public EdiObject(String openapi, EdiInfo info, EdiComponents components) {
        this.openapi   = openapi;
        this.info      = info;
        this.components= components;
    }

    public String getOpenapi() {
        return openapi;
    }

    public EdiInfo getInfo() {
        return info;
    }

    public EdiComponents getComponents() {
        return components;
    }
}

