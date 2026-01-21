package com.card.management.DTOs;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class CardSearchRequestDto {
  @NotNull(message = "Account number not provided")
  private Long accountId;

  @NotBlank(message = "Card number not provided")
  private String cardNumber;
}
