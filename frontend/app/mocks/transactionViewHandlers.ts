// app/mocks/transactionViewHandlers.ts (exacto al backend Spring Boot)
import { http, HttpResponse } from 'msw';
import type { TransactionViewRequest, TransactionViewResponse } from '~/types/transactionView';

// ✅ Datos mock que coinciden exactamente con TransactionRecord del backend
const mockTransactionRecords: Record<string, any> = {
  '1000000000001': {
    transactionId: '1000000000001',
    cardNumber: '4111111111111111',
    transactionTypeCode: '01',
    transactionCategoryCode: '5411',
    transactionSource: 'ONLINE',
    transactionAmount: '125.50',              // ✅ BigDecimal como string
    transactionDescription: 'GROCERY STORE PURCHASE - SUPERMARKET XYZ',
    originalTimestamp: '2024-01-15T10:30:00', // ✅ LocalDateTime format
    processedTimestamp: '2024-01-15T10:31:00', // ✅ LocalDateTime format
    merchantId: '987654321',
    merchantName: 'SUPERMARKET XYZ',
    merchantCity: 'NEW YORK',
    merchantZip: '10001',
  },
  '1000000000002': {
    transactionId: '1000000000002',
    cardNumber: '4222222222222222',
    transactionTypeCode: '02',
    transactionCategoryCode: '5542',
    transactionSource: 'POS',
    transactionAmount: '75.25',
    transactionDescription: 'FUEL PURCHASE - SHELL STATION',
    originalTimestamp: '2024-01-14T14:15:00',
    processedTimestamp: '2024-01-14T14:16:00',
    merchantId: '123456789',
    merchantName: 'SHELL GAS STATION',
    merchantCity: 'LOS ANGELES',
    merchantZip: '90210',
  },
  '1000000000003': {
    transactionId: '1000000000003',
    cardNumber: '4333333333333333',
    transactionTypeCode: '03',
    transactionCategoryCode: '6011',
    transactionSource: 'ATM',
    transactionAmount: '-200.00',
    transactionDescription: 'CASH WITHDRAWAL - ATM TRANSACTION',
    originalTimestamp: '2024-01-13T16:45:00',
    processedTimestamp: '2024-01-13T16:46:00',
    merchantId: '999888777',
    merchantName: 'BANK ATM',
    merchantCity: 'MIAMI',
    merchantZip: '33101',
  },
};

// ✅ Función que replica exactamente TransactionViewService.populateHeaderInfo()
const populateHeaderInfo = (): Pick<TransactionViewResponse, 'currentDate' | 'currentTime' | 'programName' | 'transactionName'> => {
  const now = new Date();
  
  return {
    currentDate: now.toLocaleDateString('en-US', { 
      year: '2-digit', 
      month: '2-digit', 
      day: '2-digit' 
    }), // MM/dd/yy format
    currentTime: now.toLocaleTimeString('en-US', { 
      hour12: false,
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit'
    }), // HH:mm:ss format
    programName: 'COTRN01C',
    transactionName: 'CT01',
  };
};

// ✅ Función que replica exactamente TransactionViewService.initializeEmptyView()
const createEmptyResponse = (): TransactionViewResponse => {
  return {
    ...populateHeaderInfo(),
  };
};

export const transactionViewHandlers = [
  // ✅ Endpoint exacto: @PostMapping("/transaction/search")
  http.post('/api/transaction/search', async ({ request }) => {
    const body = await request.json() as { transactionId: string };
    
    console.log('🔍 Transaction View Search Request (MSW):', body);

    // Simular delay de red
    await new Promise(resolve => setTimeout(resolve, 800));

    // ✅ Validación exacta del backend: TransactionViewService.getTransactionDetails()
    if (!body.transactionId || body.transactionId.trim() === '') {
      return HttpResponse.json({
        ...createEmptyResponse(),
        errorMessage: 'Transaction ID cannot be empty...',
      } as TransactionViewResponse);
    }

    const transactionId = body.transactionId.trim();

    try {
      // ✅ Simular: Optional<TransactionRecord> transactionOpt = transactionRecordRepository.findById(transactionId)
      const transactionRecord = mockTransactionRecords[transactionId];
      
      if (transactionRecord) {
        console.log('✅ Transaction found (MSW):', transactionId);
        
        // ✅ Mapeo exacto como en el backend
        const response: TransactionViewResponse = {
          transactionId: transactionRecord.transactionId,
          cardNumber: transactionRecord.cardNumber,
          transactionTypeCode: transactionRecord.transactionTypeCode,
          transactionCategoryCode: transactionRecord.transactionCategoryCode,
          transactionSource: transactionRecord.transactionSource,
          transactionAmount: transactionRecord.transactionAmount,
          transactionDescription: transactionRecord.transactionDescription,
          originalTimestamp: transactionRecord.originalTimestamp,
          processedTimestamp: transactionRecord.processedTimestamp,
          merchantId: transactionRecord.merchantId,
          merchantName: transactionRecord.merchantName,
          merchantCity: transactionRecord.merchantCity,
          merchantZip: transactionRecord.merchantZip,
          ...populateHeaderInfo(),
        };
        
        return HttpResponse.json(response);
      } else {
        // ✅ Equivalente a DFHRESP(NOTFND) del backend
        console.log('❌ Transaction not found (MSW):', transactionId);
        return HttpResponse.json({
          ...createEmptyResponse(),
          errorMessage: 'Transaction ID NOT found...',
        } as TransactionViewResponse);
      }
    } catch (error) {
      // ✅ Equivalente al catch del backend
      console.error('❌ Error retrieving transaction (MSW):', error);
      return HttpResponse.json({
        ...createEmptyResponse(),
        errorMessage: 'Unable to lookup Transaction...',
      } as TransactionViewResponse);
    }
  }),

  // ✅ Endpoint exacto: @PostMapping("/transaction/clear")
  http.post('/api/transaction/clear', async () => {
    console.log('🧹 Transaction View Clear Request (MSW)');
    
    // Simular delay
    await new Promise(resolve => setTimeout(resolve, 300));
    
    // ✅ Equivalente exacto a TransactionViewService.initializeEmptyView()
    return HttpResponse.json(createEmptyResponse());
  }),

  // ✅ Endpoint exacto: @GetMapping("/transaction/view") (opcional para inicialización)
  http.get('/api/transaction/view', async ({ request }) => {
    const url = new URL(request.url);
    const transactionId = url.searchParams.get('transactionId');
    
    console.log('🔍 Transaction View GET Request (MSW):', transactionId);
    
    if (transactionId) {
      // Reutilizar la lógica de búsqueda
      const transactionRecord = mockTransactionRecords[transactionId];
      if (transactionRecord) {
        const response: TransactionViewResponse = {
          transactionId: transactionRecord.transactionId,
          cardNumber: transactionRecord.cardNumber,
          transactionTypeCode: transactionRecord.transactionTypeCode,
          transactionCategoryCode: transactionRecord.transactionCategoryCode,
          transactionSource: transactionRecord.transactionSource,
          transactionAmount: transactionRecord.transactionAmount,
          transactionDescription: transactionRecord.transactionDescription,
          originalTimestamp: transactionRecord.originalTimestamp,
          processedTimestamp: transactionRecord.processedTimestamp,
          merchantId: transactionRecord.merchantId,
          merchantName: transactionRecord.merchantName,
          merchantCity: transactionRecord.merchantCity,
          merchantZip: transactionRecord.merchantZip,
          ...populateHeaderInfo(),
        };
        return HttpResponse.json(response);
      } else {
        return HttpResponse.json({
          ...createEmptyResponse(),
          errorMessage: 'Transaction ID NOT found...',
        } as TransactionViewResponse);
      }
    }
    
    return HttpResponse.json(createEmptyResponse());
  }),
];