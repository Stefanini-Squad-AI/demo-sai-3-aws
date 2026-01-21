// com/card/management/Controllers/BillPaymentController.java
package com.card.management.Controllers;

import com.card.management.DTOs.BillPaymentRequestDto;
import com.card.management.DTOs.BillPaymentResponseDto;
import com.card.management.Services.BillPaymentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import java.util.Map;
import java.util.HashMap;

@RestController
@RequestMapping("/api/bill-payment")
@RequiredArgsConstructor
@Tag(name = "Bill Payment", description = "API para procesamiento de pagos de facturas")
public class BillPaymentController {
  private final BillPaymentService billPaymentService;

  @PostMapping("/process")
  @Operation(summary = "Procesar pago de factura", description = "Procesa el pago completo del saldo de una cuenta")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Pago procesado exitosamente"),
      @ApiResponse(responseCode = "400", description = "Datos de entrada inválidos"),
      @ApiResponse(responseCode = "404", description = "Cuenta no encontrada"),
      @ApiResponse(responseCode = "500", description = "Error interno del servidor")
  })
  public ResponseEntity<Map<String, Object>> processBillPayment(
      @RequestBody BillPaymentRequestDto request) {

    BillPaymentResponseDto response = billPaymentService.processBillPayment(request);

    Map<String, Object> result = new HashMap<>();
    result.put("success", response.isSuccess());
    result.put("data", response);

    if (response.isSuccess()) {
      return ResponseEntity.ok(result);
    } else {
      result.put("error", response.getErrorMessage());
      return ResponseEntity.badRequest().body(result);
    }
  }

  @GetMapping("/consult")
  public ResponseEntity<Map<String, Object>> consultBillPayment(
      @RequestParam("accountId") Long accountId) {

    BillPaymentResponseDto response = billPaymentService.consultBalance(accountId);

    Map<String, Object> result = new HashMap<>();

    if (response != null) {
      result.put("success", response.isSuccess());
      result.put("data", response);

      if (response.isSuccess()) {
        return ResponseEntity.ok(result);
      } else {
        result.put("error", response.getErrorMessage());
        return ResponseEntity.badRequest().body(result);
      }
    } else {
      result.put("success", false);
      result.put("error", "Account not found");
      return ResponseEntity.status(HttpStatus.NOT_FOUND).body(result);
    }
  }
}