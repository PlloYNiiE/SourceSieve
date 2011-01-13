#include "clang/Basic/TargetInfo.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/AST.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/SourceSieve/SourceSieve.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Path.h"
using namespace clang;
using namespace llvm;

namespace SourceSieve {
  static ASTContext *Ctx1;
  
  static DiagnosticOptions diagnosticOptions;
  static TextDiagnosticPrinter *pTextDiagnosticPrinter;
  static llvm::IntrusiveRefCntPtr<clang::DiagnosticIDs> diags;
  static Diagnostic *diagnostic;
  
  static FileSystemOptions fileSystemOpts;
  static TargetOptions targetOptions;
  static FileManager *fileManager;
  	
  //  static 
  
  static LangOptions LOpts; 
  static SourceManager *SM; 
  static TargetInfo *t; 
  static IdentifierTable *idents; 
  static SelectorTable *sels; 
  static Builtin::Context *builtins;
  
  static unsigned File_count = 0;
  static double CompareDecl(Decl *D1, Decl *D2){
    if(VarDecl *VD = dyn_cast<VarDecl>(D2)) {
      D2->dump();
      NamedDecl *ND = dyn_cast<NamedDecl>(D2);
      llvm::outs() << ND->getNameAsString() << "\n";
      // handle variable declaration
    }
    else if(FunctionDecl *FD = dyn_cast<FunctionDecl>(D2)) {
      if(FD->isMain()) {
        //D2->dump();
        NamedDecl *ND = dyn_cast<NamedDecl>(D2);
        llvm::outs() << ND->getNameAsString() << "\n";
        
        Stmt *S = FD->getBody();
        Stmt::child_iterator CI = S->child_begin(), CE = S->child_end();
        for( ; CI != CE; CI++) {
          Stmt *ChildStmt = *CI;
          //ChildStmt->dump();
          
          if(CallExpr *CEE = dyn_cast<CallExpr>(ChildStmt)) {
            
          }
          else if(ForStmt *FS = dyn_cast<ForStmt>(ChildStmt)) {
            ChildStmt->dump();
          }
          
        }
        
        
        
        
      }
      // handle function declaration
    }
    
    /*if(isa<FunctionDecl>(D2)) {
      //llvm::outs() << "D1 = ";
      //D1->dump();
      llvm::outs() << "D2 = ";
      D2->dump();
      llvm::outs() << "D1 = ";
      D1->dump();
    }*/
    return 1.0;
  }
  
}  

using namespace SourceSieve;



void SourceSieveConsumer::HandleTranslationUnit(ASTContext &Ctx) {
  llvm::outs() << "SourceSieve plugin test\n"; 
  unsigned file_count = 0;
  
  if(/*File_count==*/0){

    pTextDiagnosticPrinter = new TextDiagnosticPrinter( llvm::outs(),
    			diagnosticOptions);
    diagnostic = new Diagnostic(diags, pTextDiagnosticPrinter);

    fileManager = new FileManager(fileSystemOpts);

    LOpts = Ctx.getLangOptions();
    SM = new SourceManager(*diagnostic, *fileManager);
    targetOptions.Triple = llvm::sys::getHostTriple();
    t = TargetInfo::CreateTargetInfo(*diagnostic, targetOptions);
    idents = new IdentifierTable(LOpts);
    idents->setExternalIdentifierLookup (Ctx.Idents.getExternalIdentifierLookup ());
    //idents = new IdentifierTable(Ctx.Idents);
    sels = new SelectorTable();//Ctx.Selectors; 
    builtins = new Builtin::Context(*t);
    
    //Ctx1 = new ASTContext(LOpts, *SM, *t, *idents, *sels, *builtins, 999999);

    Ctx1 = new ASTContext(CI->getLangOpts(),
                          CI->getPreprocessor().getSourceManager(),
                          CI->getTarget(),
                          CI->getPreprocessor().getIdentifierTable(),
                          CI->getPreprocessor().getSelectorTable(),
                          CI->getPreprocessor().getBuiltinInfo(),
                          0 /**/);
    // TODO: preserve DeclContext of this ASTContext
    // XXX: in ReleaseDeclContextMaps method
    Ctx1->LastSDM = Ctx.LastSDM;
    Ctx1->TUDecl = Ctx.getTranslationUnitDecl();

    File_count++;
  }
  else {
  DeclContext::decl_iterator Dit1 /*= Ctx1->getTranslationUnitDecl()->decls_begin()*/;
  DeclContext::decl_iterator Dit2 = Ctx.getTranslationUnitDecl()->decls_begin();
  for ( ; Dit2 != Ctx.getTranslationUnitDecl()->decls_end(); /*++Dit1,*/++Dit2) {
    CompareDecl(*Dit1, *Dit2);
  }
}
 
}
