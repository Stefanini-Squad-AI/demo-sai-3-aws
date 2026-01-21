package com.card.management.DTOs;

import lombok.Data;
import jakarta.validation.constraints.*;
import com.card.management.Models.Card.CardStatus;

@Data
public class CardUpdateRequestDto {
  @NotNull(message = "Account ID is required")
  @Min(value = 1, message = "Account number must be a non zero 11 digit number")
  @Max(value = 99999999999L, message = "Account number must be a non zero 11 digit number")
  private Long accountId;

  @NotBlank(message = "Card number is required")
  @Pattern(regexp = "\\d{16}", message = "Card number must be a 16 digit number")
  private String cardNumber;

  @NotBlank(message = "Card name is required")
  @Pattern(regexp = "^[a-zA-Z\\s]+$", message = "Card name can only contain alphabets and spaces")
  @Size(max = 50)
  private String embossedName;

  @NotNull(message = "Card Active Status is required")
  private CardStatus activeStatus;

  @NotNull(message = "Expiry month is required")
  @Min(value = 1, message = "Card expiry month must be between 1 and 12")
  @Max(value = 12, message = "Card expiry month must be between 1 and 12")
  private Integer expiryMonth;

  @NotNull(message = "Expiry year is required")
  @Min(value = 1950, message = "Invalid card expiry year")
  @Max(value = 2099, message = "Invalid card expiry year")
  private Integer expiryYear;

  // Campo para el día (fijo en 01 como en el COBOL original)
  private Integer expiryDay = 1;
}
