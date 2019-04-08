package org.sentilo.web.catalog.format.misc;

public class S3Url {

  private String scheme;
  private String authority;
  private String bucket;
  private String filename;
  private boolean validUrl;

  public S3Url(final boolean valid) {
    validUrl = valid;
  }

  public S3Url(final String scheme, final String authority, final String bucket, final String filename) {
    this(true);
    this.scheme = scheme;
    this.authority = authority;
    this.bucket = bucket;
    this.filename = filename;
  }

  public String getEndpoint() {
    return scheme + "://" + authority;
  }

  public String getAuthority() {
    return authority;
  }

  public String getBucket() {
    return bucket;
  }

  public String getFilename() {
    return filename;
  }

  public boolean isValid() {
    return validUrl;
  }

}
