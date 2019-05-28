F90 = pgf90
FFLAGS = -O3 -byteswapio
#F90 = xlf90
#FFLAGS = -O3 -qstrict 
LDFLAGS = $(FFLAGS)

OBJS = gts_decoder.o \
bulletin_handler.o \
code_tables.o \
decode_airep.o \
decode_fm12.o \
decode_fm15.o \
decode_fm32.o \
decode_fm35.o \
decode_fm42.o \
decode_fm86.o \
decode_fm88.o \
mod_bulletin_def.o \
mod_date_pack.o \
mod_mm5obs_def.o \
mod_nan_arithmetic.o \
mod_parameters.o \
mod_record_def.o \
mod_special_symbols.o \
wr_rap_airep.o \
wr_rap_fm12.o \
wr_txt_fm12.o \
wr_rap_fm15.o \
wr_rap_fm35.o \
wr_rap_fm86.o \
wr_rap_fm88.o \
wr_rawins_fm12.o \
wr_rawins_fm35.o \
wr_rawins_fm86.o \
wr_rawins_fm88.o

.SUFFIXES:
.SUFFIXES: .f90 .o

gts_decoder:	$(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS)

bulletin_handler.o:	bulletin_handler.f90 \
			mod_bulletin_def.o   \
			mod_record_def.o     \
			mod_date_pack.o
	$(F90) $(FFLAGS) -c $<

mod_nan_arithmetic.o:	mod_nan_arithmetic.f90 \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

mod_bulletin_def.o:	mod_bulletin_def.f90 \
			mod_parameters.o     \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

mod_record_def.o:	mod_record_def.f90   \
			mod_nan_arithmetic.o \
			mod_parameters.o     \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<
			
code_tables.o:	code_tables.f90    \
		code_tables.fi     \
		mod_record_def.o
	$(F90) $(FFLAGS) -c $<

gts_decoder.o:	gts_decoder.f90    \
		mod_record_def.o   \
		mod_mm5obs_def.o   \
		mod_bulletin_def.o \
		mod_date_pack.o
	$(F90) $(FFLAGS) -c $<

decode_airep.o:	decode_airep.f90   \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm12.o:	decode_fm12.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm15.o:	decode_fm15.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm32.o:	decode_fm32.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm35.o:	decode_fm35.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm42.o:	decode_fm42.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm86.o:	decode_fm86.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<

decode_fm88.o:	decode_fm88.f90    \
		mod_bulletin_def.o \
		mod_record_def.o   
	$(F90) $(FFLAGS) -c $<


wr_rap_airep.o:	wr_rap_airep.f90 \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_rap_fm12.o:	wr_rap_fm12.f90  \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_txt_fm12.o:	wr_txt_fm12.f90  \
		mod_record_def.o
	$(F90) $(FFLAGS) -c $<

wr_rap_fm15.o:	wr_rap_fm15.f90  \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_rap_fm35.o:	wr_rap_fm35.f90  \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_rap_fm86.o:	wr_rap_fm86.f90  \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_rap_fm88.o:	wr_rap_fm88.f90  \
		mod_record_def.o \
		mod_mm5obs_def.o
	$(F90) $(FFLAGS) -c $<

wr_rawins_fm12.:o	wr_rawins_fm12.f90  \
			rawins_formats.fi   \
			mod_record_def.o    \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

wr_rawins_fm35.o:	wr_rawins_fm35.f90  \
			rawins_formats.fi   \
			mod_record_def.o    \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

wr_rawins_fm86.o:	wr_rawins_fm86.f90  \
			rawins_formats.fi   \
			mod_record_def.o    \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

wr_rawins_fm88.o:	wr_rawins_fm88.f90  \
			rawins_formats.fi   \
			mod_record_def.o    \
			mod_special_symbols.o
	$(F90) $(FFLAGS) -c $<

clean:
	rm -f gts_decoder *.mod *.o

.f90.o:
	$(F90) $(FFLAGS) -c $<
