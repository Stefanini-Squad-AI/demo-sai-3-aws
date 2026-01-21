package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import jakarta.validation.constraints.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionAddRequestDto {
  // Campos de identificación - uno de los dos debe estar presente
  private String accountId;
  private String cardNumber;

  @NotBlank(message = "Type CD can NOT be empty...")
  @Pattern(regexp = "\\d{1,2}", message = "Type CD must be Numeric...")
  private String transactionTypeCode;

  @NotNull(message = "Category CD can NOT be empty...")
  @Min(value = 1, message = "Category CD must be at least 1")
  @Max(value = 9999, message = "Category CD must be at most 9999")
  private Integer transactionCategoryCode;

  @NotBlank(message = "Source can NOT be empty...")
  private String transactionSource;

  @NotBlank(message = "Description can NOT be empty...")
  private String transactionDescription;

  @NotNull(message = "Amount can NOT be empty...")
  @DecimalMin(value = "-99999999.99", message = "Amount should be in format -99999999.99")
  @DecimalMax(value = "99999999.99", message = "Amount should be in format -99999999.99")
  private BigDecimal transactionAmount;

  @NotNull(message = "Orig Date can NOT be empty...")
  private LocalDateTime originalDate;

  @NotNull(message = "Proc Date can NOT be empty...")
  private LocalDateTime processDate;

  @NotBlank(message = "Merchant ID can NOT be empty...")
  @Pattern(regexp = "\\d+", message = "Merchant ID must be Numeric...")
  private String merchantId;

  @NotBlank(message = "Merchant Name can NOT be empty...")
  private String merchantName;

  @NotBlank(message = "Merchant City can NOT be empty...")
  private String merchantCity;

  @NotBlank(message = "Merchant Zip can NOT be empty...")
  private String merchantZip;

  @NotBlank(message = "Confirm to add this transaction...")
  @Pattern(regexp = "[YyNn]", message = "Invalid value. Valid values are (Y/N)...")
  private String confirmation;
}
