package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.math.BigDecimal;
import java.time.LocalDate;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AccountUpdateRequestDto {
  private Long accountId;
  private String activeStatus;
  private BigDecimal currentBalance;
  private BigDecimal creditLimit;
  private BigDecimal cashCreditLimit;
  private LocalDate openDate;
  private LocalDate expirationDate;
  private LocalDate reissueDate;
  private BigDecimal currentCycleCredit;
  private BigDecimal currentCycleDebit;
  private String groupId;

  // Customer data for update
  private Long customerId;
  private String firstName;
  private String middleName;
  private String lastName;
  private String addressLine1;
  private String addressLine2;
  private String addressLine3;
  private String stateCode;
  private String countryCode;
  private String zipCode;
  private String phoneNumber1;
  private String phoneNumber2;
  private String ssn;
  private String governmentIssuedId;
  private LocalDate dateOfBirth;
  private String eftAccountId;
  private String primaryCardIndicator;
  private Integer ficoScore;
}
