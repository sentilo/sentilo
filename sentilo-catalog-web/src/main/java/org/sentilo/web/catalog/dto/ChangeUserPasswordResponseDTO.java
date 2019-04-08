package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.springframework.validation.ObjectError;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class ChangeUserPasswordResponseDTO {

  private String result;

  @JsonInclude(value = Include.NON_EMPTY)
  private List<String> errors;

  public ChangeUserPasswordResponseDTO(final String result) {
    this.result = result;
  }

  public ChangeUserPasswordResponseDTO(final String result, final List<String> errors) {
    this(result);
    this.errors = errors;
  }

  public void addErrorMessage(final ObjectError error) {
    if (errors == null) {
      errors = new ArrayList<String>();
    }

    errors.add(error.getCode());
  }

  public String getResult() {
    return result;
  }

  public List<String> getErrors() {
    return errors;
  }

}
