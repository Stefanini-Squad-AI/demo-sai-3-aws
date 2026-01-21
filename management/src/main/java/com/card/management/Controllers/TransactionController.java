package com.card.management.Controllers;

import com.card.management.DTOs.TransactionAddRequestDto;
import com.card.management.DTOs.TransactionAddResponseDto;
import com.card.management.Services.TransactionAddService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/transactions")
@RequiredArgsConstructor
@Validated
@Slf4j
@Tag(name = "Transaction Management", description = "Operations for managing card transactions")
public class TransactionController {
  private final TransactionAddService transactionAddService;

  @PostMapping
  @Operation(summary = "Add new transaction", description = "Add a new transaction to the TRANSACT file")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Transaction added successfully"),
      @ApiResponse(responseCode = "400", description = "Invalid input data"),
      @ApiResponse(responseCode = "500", description = "Internal server error")
  })
  public ResponseEntity<TransactionAddResponseDto> addTransaction(
      @Valid @RequestBody TransactionAddRequestDto request) {

    log.info("Received request to add transaction for account: {} or card: {}",
        request.getAccountId(), request.getCardNumber());

    TransactionAddResponseDto response = transactionAddService.addTransaction(request);

    if (response.isSuccess()) {
      return ResponseEntity.ok(response);
    } else {
      return ResponseEntity.badRequest().body(response);
    }
  }
}
