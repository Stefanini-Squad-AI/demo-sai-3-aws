package com.card.management.DTOs;

import lombok.Data;

@Data
public class StatementGenerationResultDto {
  private Long accountId;
  private String textStatement;
  private String htmlStatement;
  private boolean success;
  private String errorMessage;
}
