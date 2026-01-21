package com.card.management.Controllers;

import com.card.management.DTOs.TransactionViewResponseDto;
import com.card.management.Services.TransactionViewService;
import lombok.RequiredArgsConstructor;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

/**
 * Controlador para la visualización de transacciones
 * Migrado desde el programa principal COTRN01C y sus funciones de navegación
 */
@RequestMapping("api/transaction-view")
@RequiredArgsConstructor
@RestController
public class TransactionViewController {
  private final TransactionViewService transactionViewService;

  /**
   * Muestra la pantalla inicial de visualización de transacciones
   * Equivalente a la inicialización del programa COBOL
   */
  @GetMapping("/view")
  public String showTransactionView(
      @RequestParam(value = "transactionId", required = false) String transactionId,
      Model model) {

    TransactionViewResponseDto dto;

    if (transactionId != null && !transactionId.trim().isEmpty()) {
      // Equivalente a cuando se recibe CDEMO-CT01-TRN-SELECTED
      dto = transactionViewService.getTransactionDetails(transactionId);
    } else {
      // Equivalente a la inicialización con LOW-VALUES
      dto = transactionViewService.initializeEmptyView();
    }

    model.addAttribute("transaction", dto);
    return "transaction/view"; // Vista Thymeleaf
  }

  /**
   * Procesa la búsqueda de transacción
   * Equivalente a PROCESS-ENTER-KEY del COBOL
   */
  @GetMapping("/search")
  public TransactionViewResponseDto searchTransaction(@RequestParam("transactionId") String transactionId) {

   return transactionViewService.getTransactionDetails(transactionId);

  }

  /**
   * Limpia la pantalla actual
   * Equivalente a CLEAR-CURRENT-SCREEN (PF4)
   */
  @PostMapping("/clear")
  public String clearScreen(Model model) {
    TransactionViewResponseDto dto = transactionViewService.initializeEmptyView();
    model.addAttribute("transaction", dto);
    return "transaction/view";
  }

  /**
   * Retorna al menú principal
   * Equivalente a las funciones PF3 y PF5 del COBOL
   */
  @GetMapping("/back")
  public String returnToMenu(@RequestParam(value = "target", defaultValue = "menu") String target) {
    // Equivalente a RETURN-TO-PREV-SCREEN
    switch (target) {
      case "list":
        return "redirect:/transaction/list"; // Equivalente a COTRN00C
      case "menu":
      default:
        return "redirect:/menu"; // Equivalente a COMEN01C
    }
  }
}
