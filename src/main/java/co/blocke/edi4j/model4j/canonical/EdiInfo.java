// EdiInfo.java
package co.blocke.edi4j.model4j.canonical;

public class EdiInfo {
    private final String title;
    private final String version;

    public EdiInfo(String title, String version) {
        this.title = title;
        this.version = version;
    }

    public String getTitle() {
        return title;
    }

    public String getVersion() {
        return version;
    }
}

