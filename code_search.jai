#import "Basic";
#import "Window_Creation";
#import "Input";
#import "Simp";
#import "GetRect_LeftHanded";
#import "String";
#import "File";
#import "File_Utilities";
#import "Thread";
#import "Math";
#import "Hash_Table";
#import "Text_File_Handler";
#import "System";

DEBUG :: false;

window_width  : s32 = 1920;
window_height : s32 = 1080;

theme: Overall_Theme;

TheFont: *Dynamic_Font;
FontSize := 18;
LineHeight := 0.0;

LoadFont :: () {
    TheFont = get_font_at_size("data", "ProggyVector-Regular.ttf", FontSize);
    LineHeight = xx TheFont.character_height * 1.5;
}

scroll_region_value: float;
scroll_region_value_for_file_contents: float;
scroll_region_value_for_search_dirs: float;

ClearQueryResults :: () {
    lock(*QueryResultsMutex);
    array_reset(*QueryResults);
    unlock(*QueryResultsMutex);
}

Line :: struct {
	index := -1;
	text: string;
}

LoadedFile :: struct {
    name: string;
    path: string;
    contents: string;
    lines: [..]Line;
    loaded := false;
}

FreeLoadedFile :: (file: LoadedFile) {
	free(file.name);
	free(file.path);
	free(file.contents);
	array_reset(*file.lines);
}

QueryResult :: struct {
	file: *LoadedFile;
    line_index := -1;
}
QueryResults: [..]QueryResult;


QueryResultsMutex: Mutex;
QueryMutex: Mutex;
SearchEnvironmentMutex: Mutex;

DirectoryState :: struct {
	files: [..]LoadedFile;
	initialized := false;
	scanned := false;
	removed := false;
}


QueryThreadInitialized := false;
QueryThread: Thread;
QueryThreadProc :: (thread: *Thread) -> s64 {
	LastQuery: string;
	LastSearchEnvironment: string;

	Filetypes: [..]string;
	Directories: Table(string, DirectoryState);

	while true {
        lock(*SearchEnvironmentMutex);
        search_environment_updated := LastSearchEnvironment != SearchEnvironment;
        if search_environment_updated then modify_string(*LastSearchEnvironment, SearchEnvironment);
        unlock(*SearchEnvironmentMutex);

        if search_environment_updated {
        	auto_release_temp();

        	ClearQueryResults();

        	search_filetypes: [..]string;
        	search_filetypes.allocator = temp;
        	search_directories: [..]string;
        	search_directories.allocator = temp;

        	for search_env_piece: split(LastSearchEnvironment, ";",, temp) {
        		log("Checking piece: %", search_env_piece);

        		if trim(search_env_piece).count == 0 {
        			continue;
        		}

        		if search_env_piece[0] == #char "." {
        			array_add(*search_filetypes, search_env_piece);
        			// if !array_find(Filetypes, search_env_piece) {
        			// 	log("Adding file type %", search_env_piece);
    				// 	array_add(*Filetypes, copy_string(search_env_piece));
    				// 	filetypes_added = true;
	            	// 	continue;
        			// }
        		} else if file_exists(search_env_piece) {
        			log("% is a directory", search_env_piece);
        			array_add(*search_directories, search_env_piece);
        			// if !table_contains(*Directories, search_env_piece) {
        			// 	table_add(*Directories, search_env_piece, .{});
        			// 	directories_added = true;
        			// }
        		} else {
        			log_error("Unknown env piece: %", search_env_piece);
        		}
        	}

        	filetypes_changed := false;
        	removed_filetypes: [..]string;
        	removed_filetypes.allocator = temp;
        	added_filetypes: [..]string;
        	added_filetypes.allocator = temp;

        	// check file types
        	for filetype: search_filetypes {
        		if !array_find(Filetypes, filetype) {
        			log("Filetype added: %", filetype);
        			array_add(*Filetypes, copy_string(filetype));
        			array_add(*added_filetypes, filetype);
        			filetypes_changed = true;
        		}
        	}

        	for filetype: Filetypes {
        		if !array_find(search_filetypes, filetype) {
        			log("Filetype removed: %", filetype);
        			free(filetype);
        			array_ordered_remove_by_index(*Filetypes, it_index);
        			array_add(*removed_filetypes, filetype);
        			filetypes_changed = true;
        		}
        	}



        	// check directories
        	for value, key: Directories {
        		if !array_find(search_directories, key) {
        			log("Directory removed: %", key);
        			// remove the directory
        			assert(table_remove(*Directories, key), "Failed to remove file!");
        			free(key);
        			for file: value.files {
        				FreeLoadedFile(file);
        			}
        		}
        	}
        	for dir: search_directories {
        		if !table_find(*Directories, dir) {
        			log("Directory added: %", dir);
        			table_add(*Directories, copy_string(dir), .{});
        		}
        	}

        	ScanDirectoryForFileWithMatchingExtensions :: (path: string, dir_state: *DirectoryState, filetype: string) {
				log("Scanning directory '%' for files with extension '%'", path, filetype);

				VisitFilesState :: struct {
					file_extension: string;
					files: *[..]LoadedFile;
				}
				visit_files_state: VisitFilesState;
				visit_files_state.file_extension = filetype;
				visit_files_state.files = *dir_state.files;

				visitor :: (info: *File_Visit_Info, state: *VisitFilesState) {
					if begins_with(info.short_name, ".") then return;
				    if !ends_with(info.short_name, state.file_extension) then return;

				    loaded_file: LoadedFile;
				    loaded_file.name = copy_string(info.short_name);
				    loaded_file.path = copy_string(info.full_name);
				    array_add(state.files, loaded_file);
				}
				visit_files(path, recursive=true, *visit_files_state, visitor, visit_directories=true);
				log("Scan complete. Added % files", dir_state.files.count);
        	}


        	for *dir_state, path: Directories {
        		if !dir_state.initialized {
        			for filetype: Filetypes {
        				ScanDirectoryForFileWithMatchingExtensions(path, dir_state, filetype);
        			}
        		} else if filetypes_changed {
        			if removed_filetypes {
        				for removed_filetype: removed_filetypes {
        					for file: dir_state.files {
        						if ends_with(file.name, removed_filetype) {
        							FreeLoadedFile(file);
        							remove file;
        						}
        					}
        				}
        			}

        			if added_filetypes {
        				for added_filetype: added_filetypes {
        					ScanDirectoryForFileWithMatchingExtensions(path, dir_state, added_filetype);
        				}
        			}
        		}
        	}
        }

		lock(*QueryMutex);
        query_updated := LastQuery != Query;
        if query_updated then modify_string(*LastQuery, Query);
        unlock(*QueryMutex);
        if query_updated && LastQuery.count > 0 {
        	query := LastQuery;
            log("Query updated: %", query);

            ClearQueryResults();

		    for dir_state, path: Directories {
		    	for file_index: 0..dir_state.files.count-1 {
			        file: *LoadedFile = *dir_state.files[file_index];
					//log("Executing Query for file: %", file.path);
				    if !file.loaded {
				        log("Loading file %", file.path);
				        file.contents, file.loaded = read_entire_file(file.path);
				        if !file.loaded then {
				            log("Failed to load file %", file.path);
				            continue;
				        }

				        dup_ptr := file.contents;
				        start_of_line_index := 0;
				        end_of_line_index := 0;
				        for index: 0..file.contents.count-1 {
				        	char := file.contents[index];
				        	if char == #char "\n" {
				        		end_of_line_index := index;
				        		line: Line;
				        		line.index = start_of_line_index;
				        		line.text = file.contents;
				        		line.text.count = index+1;
				        		advance(*line.text, start_of_line_index);
								// line.text = trim(line.text);

								// log("line %: %", file.lines.count+1, line.text);

				        		array_add(*file.lines, line);

				        		// "abcd1234\nsjdflaksdjf\n"
				        		//            ^
				        		//       start of line

				        		start_of_line_index = index+1;
				        	}
				        }
				    }
				    if file.loaded {
				    	for line: file.lines {
				    		if contains(line.text, query) {
					    		query_result: QueryResult;
					            query_result.file = file;
					            query_result.line_index = it_index;
					            lock(*QueryResultsMutex);
					            array_add(*QueryResults, query_result);
					            unlock(*QueryResultsMutex);
				    		}
				    	}
				    }
			    }
		    }
        }

        sleep_milliseconds(4);
    }

    return 0;
}

DrawQuerySelection :: (destination_rect: Rect) {
	scrollable_region_theme := theme.scrollable_region_theme;
	scrollable_region_theme.region_background.shape.rounding_flags = 0;
	scrollable_region, results_display_rect := begin_scrollable_region(destination_rect, *scrollable_region_theme);

	max_items := 250;//cast(s64) ceil(r.h / query_input_rect.h);

    result_item_rect := results_display_rect;
    result_item_rect.h = LineHeight;
	result_item_rect.y -= scroll_region_value;

	files_drawn: [..]string;
	files_drawn.allocator = temp;

    lock(*QueryResultsMutex);
    for *QueryResults {
    	file := it.file;
    	line := file.lines[it.line_index];

    	// NOTE: Keeping track of drawn file names in an array,
    	//       we check to see if the file name has been drawn 
    	//       yet. If it hasn't, then we draw the file name!
    	if !array_find(files_drawn, file.name) {
    		label(result_item_rect, file.path, *theme.label_theme);
    		result_item_rect.y += LineHeight;
    		array_add(*files_drawn, file.name);
    	}

    	button_text := tprint("L%: %", it.line_index+1, trim(line.text));
        pressed := button(result_item_rect, button_text, *theme.button_theme, identifier = it_index);
        if pressed {
            SelectedQueryResult = it;
            ScrollToSelectedLine = true;
        }
        result_item_rect.y += LineHeight;
        if it_index == max_items then break;
    }
    unlock(*QueryResultsMutex);

    end_scrollable_region(scrollable_region, 0, result_item_rect.y + result_item_rect.h, *scroll_region_value);
}

Query: string;
SelectedQueryResult: *QueryResult;
ScrollToSelectedLine := false;

SearchEnvironment: string;

// SettingsSubwindow: Subwindow_Info;
// AddDirectoryInput: string;
// DrawSettingsSubwindow :: (state: *Subwindow_State, r: Rect, data: *void) {
// 	line_height := TheFont.character_height*1.5;
// 	my_rect := r;
// 	my_rect.h = line_height;
// 	{
// 		label(my_rect, "Search Directories", *theme.label_theme);
// 	}
// 	my_rect.y += my_rect.h;

// 	{
// 		add_directory_input_rect := my_rect;
// 		result_action, new_text, state := text_input(add_directory_input_rect, AddDirectoryInput, *theme.text_input_theme);
// 	    if result_action & .TEXT_MODIFIED {
// 	        modify_string(*AddDirectoryInput, state.text);
// 	    } else if result_action & .ENTERED {
// 	    	directory_to_add := copy_string(AddDirectoryInput);
// 	    	AddSearchDirectory(directory_to_add);
// 	    	state.text = "";
// 	    	modify_string(*AddDirectoryInput, "");
// 	    }
// 	}
//     my_rect.y += my_rect.h;

// 	{
// 		search_directory_list_rect := my_rect;
// 		search_directory_list_rect.h = r.h;
// 		x_glyph_width := prepare_text(TheFont, "-") * 3;
// 		scrollable_region_theme := theme.scrollable_region_theme;
// 	    scrollable_state, scrollabe_rect :=
// 	    	begin_scrollable_region(search_directory_list_rect, *scrollable_region_theme);

// 	    search_directory_item_rect := scrollabe_rect;
// 	    search_directory_item_rect.h = line_height;
// 		search_directory_item_rect.y -= scroll_region_value_for_search_dirs;

// 		directory_label_rect := search_directory_item_rect;
// 		directory_label_rect.w -= x_glyph_width;

// 		remove_directory_button_rect := directory_label_rect;
// 		remove_directory_button_rect.x += directory_label_rect.w;
// 		remove_directory_button_rect.w = xx x_glyph_width;

// 		lock(*DirectoriesToSearchMutex);

// 			for *directory: DirectoriesToSearch {
// 				label(search_directory_item_rect, directory.path, *theme.label_theme);
// 				remove_pressed := button(remove_directory_button_rect, "-", *theme.button_theme, identifier = it_index);
// 				if remove_pressed {
// 					directory.removed = true;
// 				}
// 				search_directory_item_rect.y += search_directory_item_rect.h;
// 				remove_directory_button_rect.y += remove_directory_button_rect.h;
// 			}

// 		unlock(*DirectoriesToSearchMutex);

// 	    end_scrollable_region(scrollable_state, 0, search_directory_item_rect.y, *scroll_region_value_for_search_dirs);
// 	}
// }

AfterSearchCriteriaChanged :: () {
	SelectedQueryResult = null;
}

// TODO: Consider moving the searching logic into it's own file

main :: () {

	init(*QueryResultsMutex);
    init(*QueryMutex);
    init(*SearchEnvironmentMutex);

	exe_filepath := get_path_of_running_executable();
	exe_filepath.count = find_index_from_right(exe_filepath, "/");
	settings_filename := "code_search_state.txt";
	settings_file_filepath := sprint("%/%", exe_filepath, settings_filename);
	
	if file_exists(settings_file_filepath) {
		tfh: Text_File_Handler;
		start_file(*tfh, "settings", settings_file_filepath);
		if tfh.failed {
			log_error("Failed to load settings");
			return;
		}

		line, found := consume_next_line(*tfh);
		while found {
			Param :: (str: string) -> bool, string, string {
				arr := split(str, "=");
				if arr.count != 2 then return false, "", "";
				param := arr[0];
				value := arr[1];
				return true, param, value;
			}

			ok, param, value := Param(line);
			if ok {
				if param == "env" {
					SearchEnvironment = copy_string(value);
				} else if param == "query" {
					Query = copy_string(value);
				}
			} else {
				log_error("Unknown param '%'", line);
			}
			
			line, found = consume_next_line(*tfh);
		}

		deinit(*tfh);
	} else {
		Query = copy_string("");
	    SearchEnvironment = sprint("%;.jai;", exe_filepath);
	}

    thread_initialized := thread_init(*QueryThread, QueryThreadProc);
    if !thread_initialized {
        log_error("Failed to initialize query thread!");
        return;
    }
    thread_start(*QueryThread);

    

    window := create_window(window_width, window_height, "Code Searcher");

	set_render_target(window, .LEFT_HANDED);

	ui_init();

    LoadFont();

    // SettingsSubwindow.rect = get_rect(100, 100, 0.15 * window_width, TheFont.character_height*20.0);
	// SettingsSubwindow.title_text = "Settings";
	// SettingsSubwindow.draw = DrawSettingsSubwindow;
	// SettingsSubwindow.open = false;

    last_loop_elapsed_time: float64 = 0;
    program_is_running := true;
    while program_is_running {
        time_at_beginning_of_loop := current_time_monotonic();

		update_window_events();

		for get_window_resizes() {
			update_window(it.window);

			if it.window == window {
                window_width  = it.width;
                window_height = it.height;

                LoadFont();
                theme.font = TheFont;
            }
		}

		for events_this_frame {
			if it.type == .QUIT {
				program_is_running = false;
			} else if it.type == .KEYBOARD {
				if it.key_code == #char "+" && it.ctrl_pressed {
					FontSize += 2;
					LoadFont();
				} else if it.key_code == #char "-" && it.ctrl_pressed {
					FontSize -= 2;
					LoadFont();
				}
			}
			getrect_handle_event(it);
		}

		clear_render_target(0.25, 0.25, 0.25, 1.0);
    	x, y, width, height := get_dimensions(window, true);
    	ui_per_frame_update(window, width, height, seconds_since_init());

    	base_theme := default_theme_procs[0]();
        theme = base_theme;
        theme.font = TheFont;
        theme.button_theme.font = theme.font;
        theme.button_theme.alignment = .Left;
        theme.label_theme.font = TheFont;
        theme.label_theme.alignment = .Left;
        theme.text_input_theme.font = TheFont;
        theme.scrollable_region_theme.region_background.shape.rounding_flags = 0;
        set_default_theme(theme);

        search_directories_label_rect: Rect;
        search_directories_label_rect.w = xx prepare_text(TheFont, "Env  ");
        search_directories_label_rect.h = LineHeight;

        label(search_directories_label_rect, "Env", *theme.label_theme);

        search_directories_input_rect: Rect;
        search_directories_input_rect.x += search_directories_label_rect.w;
        search_directories_input_rect.w = (window_width - search_directories_label_rect.w);
        search_directories_input_rect.h = LineHeight;

        { // search directories input
            result_action, new_text, state := text_input(search_directories_input_rect, SearchEnvironment, *theme.text_input_theme);
            if result_action & .TEXT_MODIFIED {
            	
            } else if result_action & .ENTERED {
            	log("SearchEnvironment=%", SearchEnvironment);
            	modify_string(*SearchEnvironment, state.text);
            }
        } // search directories input

        // settings_button_rect: Rect;
        // settings_button_rect.h = TheFont.character_height*1.5;
        // settings_button_rect.w = prepare_text(TheFont, "Settings")*1.5;
        // settings_button_rect.x = window_width - settings_button_rect.w;

        query_label_rect: Rect;
        query_label_rect.y += LineHeight;
        query_label_rect.h = LineHeight;
        query_label_rect.w = xx prepare_text(TheFont, "Query  ");

        label(query_label_rect, "Query", *theme.label_theme);

        query_input_rect: Rect;
        query_input_rect.x += query_label_rect.w;
        query_input_rect.y += LineHeight;
        query_input_rect.h = LineHeight;
        query_input_rect.w = (window_width - query_label_rect.w);

        { // search text input
            result_action, new_text, state := text_input(query_input_rect, Query, *theme.text_input_theme);
            if result_action & .TEXT_MODIFIED {
                lock(*QueryMutex);
                modify_string(*Query, state.text);
                unlock(*QueryMutex);
                AfterSearchCriteriaChanged();
            }
        } // search text input

		// { // settings button
		// 	button_theme := theme.button_theme;
		// 	button_theme.alignment = .Center;
	    //     settings_button_pressed := button(settings_button_rect, "Settings", *button_theme);
	    //     if settings_button_pressed {
	    //     	SettingsSubwindow.open = true;
	    //     }
	    // } // settings button

        if SelectedQueryResult {
        	selected_file := SelectedQueryResult.file;
			selected_line := selected_file.lines[SelectedQueryResult.line_index];

        	slidable_rect: Rect;
            slidable_rect.h = window_height - (query_input_rect.h);
            slidable_rect.w = xx window_width;
            slidable_rect.y = query_input_rect.y + query_input_rect.h;

			slidable_theme := *theme.slidable_region_theme;
			slidable_theme.orientation = .VERTICAL;
            slidable_state, first_rect, second_rect, divider_r := slidable_region(slidable_rect, slidable_theme);

			DrawQuerySelection(first_rect);

            switch_to_second_rect(slidable_state);

			{ // begin selection preview section
	            selection_preview_rect := second_rect;
	            selection_preview_rect.y += 4.0; // this is global_frame_thickness

	            file_path_rect := selection_preview_rect;
				file_path_rect.h = xx TheFont.character_height;
	            label(file_path_rect, selected_file.path, *theme.label_theme);
				selection_preview_rect.y += (TheFont.character_height*1.5);
				selection_preview_rect.h -= (TheFont.character_height*1.5);

				{ // begin file contents region
					scrollable_region_theme := theme.scrollable_region_theme;
					scrollable_region_theme.region_background.shape.rounding_flags = 0;
					scrollable_region, file_contents_scrollable_rect := begin_scrollable_region(selection_preview_rect, *scrollable_region_theme);

					if ScrollToSelectedLine {
						y_value := SelectedQueryResult.line_index * TheFont.character_height;
						scroll_region_value_for_file_contents = xx y_value;
						ScrollToSelectedLine = false;
					}

					file_contents_line_rect := file_contents_scrollable_rect;
					file_contents_line_rect.h = xx TheFont.character_height;
					file_contents_line_rect.y -= scroll_region_value_for_file_contents;
					lock(*QueryResultsMutex);
					for line_index: 0..selected_file.lines.count-1 {
						file_contents_line_rect.x = file_contents_scrollable_rect.x;
						line := selected_file.lines[line_index];

						if line_index == SelectedQueryResult.line_index {
							draw_procs.set_shader_for_color();
							p0 := Vector2.{ file_contents_line_rect.x, file_contents_line_rect.y };
							p1 := p0 + .{ file_contents_line_rect.w, 0 };
							p2 := p0 + .{ file_contents_line_rect.w, file_contents_line_rect.h };
							p3 := p0 + .{ 0, file_contents_line_rect.h };
							draw_procs.immediate_quad(
								p0, p1, p2, p3,
								color = theme.background_color_bright);
							draw_procs.immediate_flush();
						}



						// TODO: Do this when the file is loaded
						current_line := trim_right(line.text);
						current_line = replace(current_line, "\t", "    ",, temp);

						// current_line := line.text;//trim_right(line.text);
						//normalized_line_text = replace(normalized_line_text, "\t", "    ");

						// if contains(current_line, #char "\t") {
						// 	space_width := prepare_text(TheFont, " ");
						// 	how_much_to_advance_line := 0;

						// 	for line_char, line_index: current_line {
						// 		if line_char != #char "\t" then break;
						// 		how_much_to_advance_line += 1;
						// 		file_contents_line_rect.x += space_width * 4;
						// 	}

						// 	advance(*current_line, how_much_to_advance_line);
						// }

						// if current_line.count > 0 && current_line[current_line.count-1] == #char "\n" {
						// 	current_line.count -= 1;
						// }

						label(file_contents_line_rect, current_line, *theme.label_theme);
						file_contents_line_rect.y += file_contents_line_rect.h;
					}
					unlock(*QueryResultsMutex);

					end_scrollable_region(scrollable_region, 0, file_contents_line_rect.y, *scroll_region_value_for_file_contents);
				} // end file contents region
			} // end selection preview section

			finish(slidable_state);
        } else {
            r: Rect;
            r.h = window_height - (query_input_rect.y + query_input_rect.h);
            r.w = xx window_width;
            r.y = query_input_rect.y + query_input_rect.h;

            DrawQuerySelection(r);
        }

        // if SettingsSubwindow.open {
        // 	add_subwindow(*SettingsSubwindow, *theme.subwindow_theme);
        // }

        draw_popups();

        swap_buffers(window);

		reset_temporary_storage();

        sleep_milliseconds(15);

        last_loop_elapsed_time = to_float64_seconds(current_time_monotonic() - time_at_beginning_of_loop);
	}

	{
		settings_sb: String_Builder;
		init_string_builder(*settings_sb);
		print_to_builder(*settings_sb, "[1]\n\n");
		print_to_builder(*settings_sb, "env=%\n", SearchEnvironment);
		print_to_builder(*settings_sb, "query=%\n", Query);

		if !write_entire_file(settings_file_filepath, builder_to_string(*settings_sb)) {
			log("Failed to update settings");
		}
	}
}

modify_string :: (ps: *string, new_value: string) {
    free(ps.*);
    ps.* = copy_string(new_value);
}
