package org.sentilo.web.catalog.dto;

public class ChangeUserPasswordDTO {

  private String userName;
  private String currentPassword;
  private String newPassword;
  private String passwordRepeat;

  public ChangeUserPasswordDTO() {
    super();
  }

  public String getCurrentPassword() {
    return currentPassword;
  }

  public void setCurrentPassword(final String currentPassword) {
    this.currentPassword = currentPassword;
  }

  public String getNewPassword() {
    return newPassword;
  }

  public void setNewPassword(final String newPassword) {
    this.newPassword = newPassword;
  }

  public String getPasswordRepeat() {
    return passwordRepeat;
  }

  public void setPasswordRepeat(final String passwordRepeat) {
    this.passwordRepeat = passwordRepeat;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(final String userName) {
    this.userName = userName;
  }

}
